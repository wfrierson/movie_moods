###############################################################################
# This script produces a set of data.tables representing selected screenplays 
# to use in this project. These screenplays will later be transformed according 
# to their "components": scene descriptions, setting descriptions, and dialogue
# from each character.

###############################################################################
# SETUP

folder.data <- '100_Data'
folder.data.raw <- file.path(folder.data, '110_Raw_Data')
folder.data.processed <- file.path(folder.data, '120_Processed_Data')
folder.code <- '200_Code'

path.dependencies <- file.path(folder.code, '000_Dependencies.R')
path.screenplayFunctions <- file.path(folder.code, '200_Screenplay_Functions.R')

source(path.dependencies)
source(path.screenplayFunctions)

###############################################################################
# GET SCREENPLAY PATHS AND GENRES

pathIMSDB <- file.path(folder.data.raw, 'imsdb_raw_nov_2015')

# Get paths to all scraped imsdb screenplays
screenplayPaths <- data.table::data.table(
  path = list.files(path = pathIMSDB, pattern = '*.txt', recursive = TRUE)
)

# Split out fields, genre in particular
screenplayPaths[, c('genre', 'filename') := data.table::tstrsplit(path, '/')]
screenplayGenres <- data.table::dcast(
  screenplayPaths, 
  filename ~ genre, 
  fun = function(x) sum(ifelse(!is.na(x), 1L, 0L)),
  value.var = 'filename'
)

# Restate files according to folders/genres containing them
screenplayPaths <- screenplayPaths[
  , .(path = min(path))
  , keyby = filename
][
  screenplayGenres, on = 'filename'
][
  # Remove following movie because it has some bad text encodings (for ease)
  # Not worth fixing...
  filename != 'mightymorphinpowerrangersthemovie.txt'
][
  , movie := gsub('.txt', '', filename, fixed = TRUE)
]

# Get vector of genre labels
genres <- colnames(screenplayPaths)[-c(1:2, 25)] %>% copy

# Create lookup table that gives cleaner list of genres per movie
screenplayGenres <- melt(
  screenplayPaths
  , id.vars = c('movie', 'filename')
  , measure.vars = genres
  , variable.name = 'genre'
  , value.name = 'genre_indicator'
  , variable.factor = FALSE
)[
  genre_indicator == 1
][
  , .(
    genreList = paste0(genre, collapse = ', ')
  )
  , keyby = movie
]

# Append simpler genre list to screenplayPaths
screenplayPaths <- screenplayGenres[screenplayPaths, on = 'movie']

###############################################################################
# IMPORT SCREENPLAYS

# The screenplays data.table is a nested list, where each element is a named
# list with the following elements:
#    * The screenplay itself represented as a data.table
#    * The name of the movie
    
screenplays <- lapply(
  seq(nrow(screenplayPaths)),
  function(index) {
    screenplay <- data.table::fread(
      file.path(pathIMSDB, screenplayPaths[index]$path)
      , sep = NULL
      , header = FALSE
      , col.names = 'string'
      , quote = ""
      , strip.white = FALSE
      , blank.lines.skip = TRUE
    )
    
    list(
      'screenplay' = screenplay,
      'movie' = screenplayPaths[index]$movie
    )
  }
)

###############################################################################
# Calculate statistics for each line and each screenplay to later infer
# which lines can be used for the following screenplay components:
#    dialogue, character labels, scene changes, and scene descriptions
#
# These features are inferred by finding patterns in indentations for each
# screenplay. Usually, scene descriptions have the fewest indents. Dialogue
# has more indents than descriptions. Character labels have more indents than
# dialogue (to denote who's speaking). Lastly, scene changes often occur with
# scene descriptions or sometimes on their own indentation level.
#
# Screenplays statistics can be calculated with or without parallel processing.
# The following commented-out code uses no parallel processing (and takes 
# 10 - 15 min), whereas the proceeding code uses parallel processing (and takes
# ~5 min). The non-parallel code can monitored by following the current movie 
# being analyzed which is printed to console.
#
# NON-PARALLEL CODE:
#
# screenplayStats <- rbindlist(lapply(
#   screenplays,
#   function(screenplayList) {
#     cat(paste0(screenplayList[[2]], '\n'))
# 
#     GetMovieTranscriptStats(
#       screenplayList[[1]]
#     )[
#       , movie := screenplayList[[2]]
#     ]
#   }
# ))

# Functions to export to each core
functionsToExport <- list(
  'GetMovieTranscriptStats',
  'CreateRegexFilter.token',
  'CreateRegexFilter'
)

# NOTE: For those reproducing this, you may need to modify the number of cores, 
# currently set at 4.
cl <- snow::makeCluster(4, type = 'SOCK')

# Exporting needed packages and functions to each core
snow::clusterCall(cl, function(x) library(data.table))
snow::clusterCall(cl, function(x) library(magrittr))
snow::clusterCall(cl, function(x) library(stringi))
snow::clusterExport(
  cl,
  functionsToExport,
  environment()
)

# Running parallel code
screenplayStats <- rbindlist(snow::parLapply(
  cl,
  screenplays,
  function(screenplayList) {
    GetMovieTranscriptStats(
      screenplayList[[1]]
    )[
      , movie := screenplayList[[2]]
    ]
  }
))
snow::stopCluster(cl)

###############################################################################
# Identify movies with regular indentation formatting in order to maximize
# confidence in well structured screenplay transformations.
screenplayIndentationStats <- screenplayStats[
  # For those not familiar with data.table, the following block does two
  # group-by's at once:
  , {
    # 1) For each movie, calculate the total number of records and tokens
    totalRecordCount = .N
    totalTokenCount = sum(tokenCount)
    
    .SD[
      # 2) For each distinct indentation (i.e., leftSpaceCount) in each movie,
      #    calculate various statistics at the token and record level.
      , .(
        pctTokens = sum(tokenCount) / totalTokenCount
        , pctRecords = .N / totalRecordCount
        , pctTokensI = sum(tokenCountI) / sum(tokenCount)
        , pctTokensQuestionMark = sum(tokenCountQuestionMark) / sum(tokenCount)
        , pctTokensQuestionWord = sum(tokenCountQuestionWord) / sum(tokenCount)
        , pctTokensYou = sum(tokenCountYou) / sum(tokenCount)
        , pctTokensWe = sum(tokenCountWe) / sum(tokenCount)
        , pctTokensCapitalized = sum(capitalizedTokenCount) / sum(tokenCount)
        , pctTokensCharacterMention = 
            sum(tokenCountCharacterMention) / sum(tokenCount)
        , pctTokensCharacterMentionCapitalized = 
            sum(tokenCountCharacterMentionCapitalized) / sum(tokenCount)
        , pctLinesCharacterMention = 
            sum(tokenCountCharacterMention) / .N
        , pctLinesCharacterMentionCapitalized = 
            sum(tokenCountCharacterMentionCapitalized) / .N
        , pctLinesOnlyCapitalizedCharacterMention =
            sum(capitalizedCharacterMentionLineInd) / .N
        , pctLinesCharacterDirection = sum(characterDirectionInd) / .N
        , pctLinesCapitalized = sum(allCapsInd) / .N
        , pctLinesSetting = sum(settingInd) / .N
      )
      , keyby = leftSpaceCount
    ]
  }
  , keyby = movie
][
  # Remove 13 screenplays where no characters could be inferred:
  #    9
  #    aladdin
  #    artistthe
  #    dawnofthedead
  #    e.t.
  #    legend
  #    mulan
  #    nightbreed
  #    nighttimethepoltergeisttreatment
  #    ninthgatethe
  #    rescuersdownunderthe
  #    starwarsthephantommenace
  #    withnailandi
  #
  # Characters are inferred by identifying lines with at most 3 tokens and
  # keeping the distinct lines that appear at least 10 times in each screenplay.
  # See for GetMovieTranscriptStats for details.
  !is.na(pctTokensCharacterMention)
][
  # Assume that left-spacing with "consistent" usage/variations of I, you, and
  # questions denote dialogue (so long as at least 1% of tokens occur).
  # More specifically, assume that any pairs of pctRecords* fields with at least
  # 0.01 records denote dialogue.
  , dialogueIndentInd := ifelse(
      pctTokens > 0.01 & (
          (pctTokensI > 0.01 & pctTokensYou > 0.01) | 
          (pctTokensI > 0.01 & pctTokensQuestionMark > 0.01) |
          (pctTokensI > 0.01 & pctTokensQuestionWord > 0.01) |
          (pctTokensYou > 0.01 & pctTokensQuestionMark > 0.01) |
          (pctTokensYou > 0.01 & pctTokensQuestionWord > 0.01) |
          (pctTokensWe > 0.01 & pctTokensI > 0.01) |
          (pctTokensWe > 0.01 & pctTokensYou > 0.01) |
          (pctTokensWe > 0.01 & pctTokensQuestionMark > 0.01) |
          (pctTokensWe > 0.01 & pctTokensQuestionWord > 0.01)
      ),
      1,
      0
  )
][
  # Assume that left-spacing that doesn't appear to be dialogue but also has
  # at least 35% of tokens mentioning inferred characters denotes character 
  # headers.
  , characterIndentInd := ifelse(
      dialogueIndentInd == 0 &
        pctTokensCharacterMention > 0.35,
      1,
      0
  )
][
  # Assume that left-spacing that isn't inferred dialogue or character headers
  # represents scene descriptions (so long as it has at least 10% lines per 
  # screenplay).
  , descriptionIndentInd := ifelse(
      dialogueIndentInd == 0 &
        characterIndentInd == 0 &
        pctRecords > 0.1,
      1,
      0
  )
][
  # Assume that left-spacing that isn't inferred dialogue or character headers 
  # but also has elements appearing to describe settings (e.g., EXT. 
  # AMY'S HOUSE) denotes setting changes.
  # a setting
  , settingIndentInd := ifelse(
      dialogueIndentInd == 0 &
        characterIndentInd == 0 &
        pctLinesSetting > 0.01,
      1,
      0
  )
][
  # Redefine character direction indicator to only accept its value if it does 
  # not occur on an inferred description line. When this component appears in a
  # description line, it tends to be a genuine parenthetical note and not
  # a direction for an actor.
  , characterDirectionIndentInd := ifelse(
      pctLinesCharacterDirection > 0.35 &
        descriptionIndentInd == 0,
      1,
      0
  )
][
  # Indicator to identify indents that look like character labels mixed with
  # dialogue. Intend to remove these screenplays because they have a very 
  # different format.
  #
  # E.g.,
  # RIPLEY
  #
  # Stay away from her!
  , mixedCharacterWithDialogueIndentInd := ifelse(
      pctLinesOnlyCapitalizedCharacterMention > 0.10 &
        dialogueIndentInd == 1 &
        pctTokens > 0.3,
      1,
      0
  )
][
  # Indicator to identify indents that look like descriptions mixed with 
  # dialogue. Intend to remove these screenplays because they have a very
  # different format.
  , mixedDescriptionWithDialogueIndentInd := ifelse(
      dialogueIndentInd == 1 &
        pctLinesCharacterMentionCapitalized > 0.1 &
        pctTokens > 0.3,
      1,
      0
  )
]

fieldsScreenplayComponents <- c(
  'dialogueIndentInd',
  'characterIndentInd',
  'descriptionIndentInd',
  'settingIndentInd',
  'characterDirectionIndentInd',
  'mixedCharacterWithDialogueIndentInd'
  ,'mixedDescriptionWithDialogueIndentInd'
)
fieldsComponentCounts <- gsub(
  'IndentInd',
  'IndentCount',
  fieldsScreenplayComponents
)

# Count number of times each screenplay component occurs for each movie
screenplayIndentationStats[
  , (fieldsComponentCounts) := lapply(.SD, sum)
  , by = movie
  , .SDcols = fieldsScreenplayComponents
][
  # Calculate additional statistics to assist in selecting well-structured
  # screenplays.
  , `:=` (
    entropy = -sum(pctTokens * log(pctTokens, base = 2))
    , pctRecordsDropped = sum(ifelse(
        dialogueIndentInd == 0 &
          characterIndentInd == 0 &
          descriptionIndentInd == 0 &
          settingIndentInd == 0,
        pctRecords,
        0
    ))
    , pctTokensDropped = sum(ifelse(
        dialogueIndentInd == 0 &
          characterIndentInd == 0 &
          descriptionIndentInd == 0 &
          settingIndentInd == 0,
        pctTokens,
        0
    ))
  )
  , by = movie
]

###############################################################################
# Create draft of well-structured screenplays where:
#    * Each screenplay component occurs at least once (269 screenplays 
#      removed),
#    * At most 5% of all tokens per screenplay are omitted by only keeping the
#      inferred components (99 additional screenplays removed), and
#    * There are no mixing of character labels and dialogue (1 additional
#      screenplays removed)
#    * There are no mixing of descriptions and dialogue (3 additional 
#      screenplays removed)
#
# Removed screenplays:
#
#    127hours, 12yearsaslave, 187, 2001aspaceodyssey, 42, 
#    44inchchest, abovethelaw, absolutepower, aceventurapetdetective, 
#    addamsfamilythe, adjustmentbureauthe, 
#    adventuresofbuckaroobanzaiacrosstheeighthdimensionthe, 
#    afterschoolspecial, agnesofgod, alienresurrection, 
#    alienvs.predator, allaboutsteve, allthekingsmen, allthepresidentsmen, 
#    amelia, americangraffiti, americanhistoryx, americanhustle, 
#    americanmadness, americanpsycho, amityvilleasylumthe, 
#    analyzethat, analyzethis, aneducation, anonymous, antz, 
#    aprilfoolsday, arbitrage, argo, armageddon, arsenicandoldlace, 
#    aseriousman, atfirstsight, authorsanonymous, avventuraltheadventure, 
#    awakenings, bachelorpartythe, baddreams, badlands, 
#    badteacher, bartonfink, battlelosangeles, battleofalgiersthe, 
#    beachthe, beastsofthesouthernwild, bestexoticmarigoldhotelthe, 
#    big, biglebowskithe, blindsidethe, bloodandwine, bluevalentine, 
#    bodyguard, bodyheat, boxthe, break, breakingaway, bridesmaids, 
#    brokenarrow, brokenembraces, brucealmighty, buried, 
#    burlesque, burningannie, cableguy, carrie, casino, 
#    catchmeifyoucan, cecilb.demented, celestejesseforever, 
#    cellular, changeupthe, chasingsleep, 
#    chroniclesofnarniathelionthewitchandthewardrobe, 
#    cirquedufreakthevampiresassistant, clashofthetitans, 
#    clueless, commando, coriolanus, cowboysaliens, crank, 
#    crazystupidlove, creation, crowcityofangelsthe, cube, 
#    darkman, datenight, davebarryscompleteguidetoguys, 
#    dayofthedead, daysofheaven, deathatafuneral, debtthe, 
#    deception, deeprising, defiance, diehard2, diner, djangounchained, 
#    dogma, dragmetohell, dragonslayer, drive, driveangry, 
#    drywhiteseasona, ducksoup, eagleeye, edwardscissorhands, 
#    elephantmanthe, elizabeththegoldenage, enough, eriktheviking, 
#    escapefroml.a., escapefromnewyork, evildead, existenz, 
#    extract, fantasticmrfox, fieldofdreams, fighterthe, 
#    findingnemo, fourfeathers, frightnight, frightnight1985, 
#    frozenriver, funnypeople, g.i.joetheriseofcobra, gamethe, 
#    gangsofnewyork, gardenstate, gattaca, getlow, ghostandthedarknessthe, 
#    ghostrider, gingersnaps, girlwiththedragontattoothe, 
#    glengarryglengross, go, godfatherpartiiithe, goodgirlthe, 
#    grandhotel, grantorino, grapesofwraththe, gravity, 
#    gremlins, gremlins2, groundhogday, grudgethe, hallpass, 
#    hancock, hanna, happybirthdaywandajune, happyfeet, 
#    hardtokill, haroldandkumargotowhitecastle, heat, heavenlycreatures, 
#    heavymetal, heist, hellboy2thegoldenarmy, highlanderendgame, 
#    hillshaveeyesthe, hitchhikersguidetothegalaxythe, horriblebosses, 
#    hospitalthe, hottubtimemachine, howtolosefriendsalienatepeople, 
#    howtotrainyourdragon, huntforredoctoberthe, iamnumberfour, 
#    informantthe, inglouriousbasterds, inthebedroom, intothewild, 
#    islandthe, ithappenedonenight, jfk, johnq, judgedredd, 
#    kafka, kids, kidsareallrightthe, kingdomthe, kingkong, 
#    kingofcomedythe, kingsspeechthe, knockedup, labyrinth, 
#    lakeplacid, landofthedead, laststationthe, lawabidingcitizen, 
#    legallyblonde, lesmiserables, lightsleeper, lincolnlawyerthe, 
#    littlemermaidthe, lockstockandtwosmokingbarrels, looper, 
#    lordoftheringsreturnoftheking, lordoftheringsthetwotowers, 
#    lordofwar, losthorizon, lostinspace, madmax2theroadwarrior, 
#    manchuriancandidatethe, manwhowasnttherethe, margaret, 
#    margotatthewedding, marleyme, marty, marypoppins, maskthe, 
#    masterandcommander, masterthe, matrixreloadedthe, maxpayne, 
#    mechanicthe, meetjohndoe, meninblack3, midnightexpress, 
#    midnightinparis, milk, miracleworkerthe, misery, moneyball, 
#    montecarlo, moon, moonrisekingdom, mrdeedsgoestotown, 
#    mutewitness, mygirl, myweekwithmarilyn, neverbeenkissed, 
#    newsies, nightmarebeforechristmasthe, ninesthe, ninotchka, 
#    nostringsattached, oblivion, obsessed, officespace, 
#    ordinarypeople, orgyofthedead, orphan, otherboleyngirlthe, 
#    pandorum, paranorman, pariah, passionofjoanofarcthe, 
#    paul, perfectcreature, perksofbeingawallflowerthe, 
#    petulia, philadelphia, phonebooth, piratesofthecaribbeandeadmanschest, 
#    platinumblonde, pokemonmewtworeturns, powerofonethe, 
#    prideandprejudice, priest, princessbridethe, 
#    privatelifeofsherlockholmesthe, producerthe, promnight, proposalthe, 
#    publicenemies, queenthe, rachelgettingmarried, realgenius, 
#    rebelwithoutacause, redplanet, redridinghood, rememberme, replacementsthe, 
#    repoman, returnoftheapes, riseoftheplanetoftheapes, 
#    roadthe, romeojuliet, roommatethe, ruinsthe, rustandbone, 
#    saintthe, saw, scarface, scottpilgrimvstheworld, semipro, 
#    serenity, serialmom, seventhsealthe, sexandthecity, 
#    sexuallife, shallowgrave, shame, shifty, shrek, siegethe, 
#    silverliningsplaybook, simone, singlewhitefemale, sixdegreesofseparation, 
#    smashed, snowwhiteandthehuntsman, socialnetworkthe, 
#    somethingsgottagive, sourcecode, spanglish, speedracer, 
#    st.elmosfire, startrekthemotionpicture, starwarsreturnofthejedi, 
#    stirofechoes, strangersonatrain, stuntmanthe, sugar, 
#    sunsetblvd., surferkingthe, surrogates, suspectzero, 
#    synecdochenewyork, syriana, takingofpelhamonetwothreethe, 
#    tamaradrewe, theragecarrie2, theressomethingaboutmary, 
#    they, thisboyslife, thisis40, thor, threemenandababy, 
#    thunderbirds, ticker, tincup, tinkertailorsoldierspy, 
#    tinmen, titanic, topgun, tristanandisolde, tropicthunder, 
#    truegrit, trueromance, trumanshowthe, twilight, twins, 
#    twoforthemoney, uglytruththe, unknown, usualsuspectsthe, 
#    vanillasky, virtuosity, wagthedog, walle, wanted, warmsprings, 
#    watchmen, waterforelephants, waybackthe, whistleblowerthe, 
#    whiteribbonthe, wildbunchthe, wildhogs, wildthingsdiamondsintherough, 
#    wizardofozthe, wolfofwallstreetthe, xmenoriginswolverine, 
#    yearone, zerodarkthirty, 
fieldsMovieLevel <- c('movie', 'entropy', 'pctRecordsDropped',
                      'pctTokensDropped', fieldsComponentCounts)
screenplaySelectionDraft <- unique(screenplayIndentationStats[
  , mget(fieldsMovieLevel)
])[
  # Each screenplay component occurs at least once
  dialogueIndentCount > 0 & 
    characterIndentCount > 0 & 
    descriptionIndentCount > 0 &
    settingIndentCount > 0 &
  
    # At most 5% of all tokens per screenplay are omitted
    pctTokensDropped < 0.05 &
    
    # There are no mixing of character labels and dialogue
    mixedCharacterWithDialogueIndentCount == 0 &
    
    # There are no mixing of descriptions and dialogue
    mixedDescriptionWithDialogueIndentCount == 0
][
  order(entropy)
]

###############################################################################
# Limit possible screenplays further by only keeping those that have exactly
# one inferred description and at most 2 character header components and 3 
# dialogue components (255 screenplays removed).
#
# Removed screenplays:
#
#    12, 12andholding, 17again, 2012, 500daysofsummer, adaptation, 
#    after.life, ali, alien3, aloneinthedark, americanbeauty, 
#    americangangster, americanpresidentthe, americanshaolinkingofkickboxersii, 
#    americanthe, angeleyes, angelsdemons, anniehall, assassins, 
#    augustosagecounty, avatar, babel, backupplanthe, badboys, 
#    baddayatblackrock, basic, basquiat, batman, batman2, 
#    bean, beinghuman, beloved, blingringthe, bluevelvet, 
#    bodiesrestmotion, bonfireofthevanities, bonnieandclyde, 
#    boogienights, bookofelithe, boondocksaints2allsaintsday, 
#    bountyhunterthe, bourneultimatumthe, brazil, breakdown, 
#    breakfastclubthe, brick, brothersbloomthe, burnafterreading, 
#    candletowater, changeling, chaos, charade, charliesangels, 
#    cherryfalls, christcomplex, chronicle, cinemaparadiso, 
#    citizenkane, cityofjoy, cobb, collateraldamage, confessionsofadangerousmind, 
#    confidence, constantine, coraline, cradle2thegrave, 
#    crazylove, crimespree, cryinggame, curiouscaseofbenjaminbuttonthe, 
#    custody, damnedunitedthe, daytheearthstoodstillthe, 
#    deathtosmoochy, departedthe, descendantsthe, despicableme2, 
#    devilinabluedress, devilsadvocate, distinguishedgentlemanthe, 
#    disturbia, donniebrasco, dumbanddumber, dune, easternpromises, 
#    edwood, eightleggedfreaks, entrapment, erinbrockovich, 
#    fairgame, fifthelementthe, finaldestination, finaldestination2, 
#    flashgordon, flintstonesthe, fracture, fridaythe13th, 
#    frozen, frozendisney, fruitvalestation, game6, gamer, 
#    getawaythe, ghostbusters, gothika, grandtheftparsons, 
#    hangoverthe, hannahandhersisters, hannibal, heathers, 
#    hebrewhammerthe, hellboy, hellraiserdeader, hellraiserhellseeker, 
#    hesjustnotthatintoyou, hisgirlfriday, honeydripper, 
#    horsewhispererthe, hotelrwanda, hudsonhawk, iloveyouphillipmorris, 
#    imaginariumofdoctorparnassusthe, inception, indianajonesandthelastcrusade, 
#    intheloop, inventingtheabbotts, inventionoflyingthe, 
#    irobot, ironladythe, itscomplicated, jacketthe, jackiebrown, 
#    jennifersbody, jerrymaguire, jimmyandjudy, jurassicpark, 
#    killbillvolume12, kramervskramer, kungfupanda, lastboyscoutthe, 
#    lastchanceharvey, lastsamuraithe, legion, liarliar, 
#    lifeofpi, littleathens, lordoftheringsfellowshipoftheringthe, 
#    malcolmx, malibusmostwanted, manhunter, manonfire, 
#    manonthemoon, mantrouble, manwhoknewtoomuchthe, margincall, 
#    mariachiel, megamind, memento, menwhostareatgoatsthe, 
#    minisfirsttime, mirrors, monkeybone, mrblandingsbuildshisdreamhouse, 
#    mrbrooks, nashville, newyorkminute, next, nextthreedaysthe, 
#    nine, ninjaassassin, observeandreport, oceanseleven, 
#    oceanstwelve, pacifierthe, papadopoulossons, perfectworlda, 
#    petsemataryii, pianothe, pineappleexpress, piratesofthecaribbean, 
#    planetoftheapesthe, postmanthe, precious, predator, 
#    purplerain, queenofthedamned, ragingbull, readerthe, 
#    reservoirdogs, revolutionaryroad, robinhoodprinceofthieves, 
#    rocknrolla, rockyhorrorpictureshowthe, roughshod, rush, 
#    s.darko, savethelastdance, schindlerslist, scream3, 
#    sessionsthe, sherlockholmes, shesoutofmyleague, shiningthe, 
#    shippingnewsthe, sleeplessinseattle, slither, slumdogmillionaire, 
#    smokinaces, snatch, southpark, spartan, startrek, startrekfirstcontact, 
#    startrekgenerations, sunshinecleaning, superbad, supergirl, 
#    sweeneytoddthedemonbarberoffleetstreet, sweethereafterthe, 
#    swordfish, takinglives, tallinthesaddle, terminatorsalvation, 
#    thelmalouise, thingsmyfathernevertaughtmethe, threemusketeersthe, 
#    timberfalls, tmnt, touristthe, trainspotting, transformersthemovie, 
#    tron, tronlegacy, twilightnewmoon, twinpeaks, unbreakable, 
#    up, valkyrie, visitorthe, walktoremembera, weownthenight, 
#    whileshewasout, whitechristmas, whitejazz, whiteout, 
#    wildthings, wildwildwest, willow, windchill, woodsmanthe, 
#    wrestlerthe, yesman, youthinrevolt, youvegotmail, zerophilia, 
#
# Note: Some text screenplays appear to have been scanned from a typed document
# or transformed from a PDF. As a result, the indentation can be inconsistent.
# I account for this by adding a modest tolerance for dialogue and character
# header components. OTOH, I noticed that screenplays with many description
# components tend to be poorly formatted overall, and so not worth transforming.
#
# There are 429 screenplays in screenplaySelection.
#
# Screenplays retained at this phase:
#
#    10thingsihateaboutyou, 12monkeys, 1492conquestofparadise, 
#    15minutes, 30minutesorless, 48hrs., 5050, 8mm, abyssthe, 
#    afewgoodmen, affliction, airforceone, airplane, airplane2thesequel, 
#    alien, aliennation, aliens, allabouteve, almostfamous, 
#    amadeus, americanpie, americansplendor, americanwerewolfinlondon, 
#    amour, anastasia, annakarenina, anniversarypartythe, 
#    antitrust, apartmentthe, apocalypsenow, aptpupil, arcade, 
#    arcticblue, armyofdarkness, arthur, asgoodasitgets, 
#    assignmentthe, austinpowersinternationalmanofmystery, 
#    austinpowersthespywhoshaggedme, autumninnewyork, avengersthe, 
#    avengersthe2012, bachelorparty, backdraft, badcountry, 
#    badlieutenant, badsanta, bamboozled, barrylyndon, basicinstinct, 
#    battleofshakerheightsthe, beavisandbuttheaddoamerica, 
#    beginners, beingjohnmalkovich, beingthere, believerthe, 
#    bigbluethe, bigfish, bigwhitethe, birdsthe, birthdaygirl, 
#    blackdahliathe, blackrain, blacksnakemoan, blackswan, 
#    blade, bladeii, bladerunner, bladetrinity, blastfromthepastthe, 
#    bloodsimple, blow, bodyofevidence, bones, boondocksaintsthe, 
#    bottlerocket, bound, bourneidentitythe, braveheart, 
#    bringingoutthedead, broadcastnews, buffythevampireslayer, 
#    bulldurham, butterflyeffectthe, capote, cars2, case39, 
#    castaway, cedarrapids, cellthe, chasingamy, chinatown, 
#    ciderhouserulesthe, cincinnatikidthe, clerks, cliffhanger, 
#    codeofsilence, coldmountain, colombiana, colorofnight, 
#    conanthebarbarian, coolerthe, copycat, crash, croodsthe, 
#    crouchingtigerhiddendragon, croupier, crowsalvationthe, 
#    crowthe, cruelintentions, danceswithwolves, darkcity, 
#    darkknightrisesthe, darkstar, daytheclowncriedthe, 
#    deadpoetssociety, deepcover, deerhunterthe, detroitrockcity, 
#    diehard, dogdayafternoon, doorsthe, dotherightthing, 
#    doubleindemnity, dropdeadgorgeous, easya, edtv, election, 
#    enemyofthestate, englishpatientthe, eternalsunshineofthespotlessmind, 
#    evencowgirlsgettheblues, eventhorizon, evildeadiideadbydawn, 
#    excalibur, fabulousbakerboysthe, faceoff, familymanthe, 
#    fantasticfour, fargo, fasttimesatridgemonthigh, fatalinstinct, 
#    faultinourstarsthe, fearandloathinginlasvegas, feast, 
#    ferrisbuellersdayoff, fightclub, fiveeasypieces, flight, 
#    forrestgump, fourrooms, frances, frankenstein, freaked, 
#    freddyvs.jason, frenchconnectionthe, frequency, 
#    fridaythe13thpartviiijasontakesmanhattan, fromdusktilldawn, 
#    fromheretoeternity, fugitivethe, g.i.jane, gandhi, gangrelated, getcarter, 
#    ghost, ghostbusters2, ghostship, ghostworld, gladiator, godfather, 
#    godfatherpartii, godsandmonsters, godzilla, gonein60seconds, 
#    goodwillhunting, graduatethe, greenmilethe, griftersthe, grossepointblank, 
#    hackers, halloweenthecurseofmichaelmyers, hardrain, 
#    hauntingthe, hellboundhellraiserii, hellraiser, hellraiser3hellonearth, 
#    helpthe, henryfool, henryscrime, hesher, highfidelity, 
#    highlander, hitchcock, hollowman, hostage, houseof1000corpses, 
#    howtotrainyourdragon2, hudsuckerproxythe, humannature, 
#    iamsam, icestormthe, idesofmarchthe, illdoanything, 
#    independenceday, indianajonesandtheraidersofthelostark, 
#    indianajonesandthetempleofdoom, indianajonesiv, insiderthe, 
#    insidious, insomnia, interviewwiththevampire, intolerablecruelty, 
#    invictus, istillknowwhatyoudidlastsummer, italianjobthe, 
#    itsawonderfullife, jacobsladder, janeeyre, jasonx, 
#    jaws, jaws2, jayandsilentbobstrikeback, jennifereight, 
#    juno, jurassicparkiii, jurassicparkthelostworld, kalifornia, 
#    kateleopold, killingzoe, klute, kundun, l.a.confidential, 
#    laboroflove, ladykillersthe, larrycrowne, lastflightthe, 
#    lastofthemohicansthe, lasttangoinparis, leavinglasvegas, 
#    leviathan, life, lifeasahouse, lifeofdavidgalethe, 
#    limeythe, limitless, lincoln, littlenicky, livinginoblivion, 
#    logansrun, lonestar, longkissgoodnightthe, lordofillusions, 
#    losersthe, losthighway, lostintranslation, loveandbasketball, 
#    machete, machinegunpreacher, magnolia, majesticthethebijou, 
#    majorleague, manhattanmurdermystery, manintheironmask, 
#    marthamarcymaymarlene, matrixthe, meanstreets, meetjoeblack, 
#    meninblack, metro, miamivice, midnightcowboy, millerscrossing, 
#    mimic, minorityreport, missionimpossible, missionimpossibleii, 
#    missiontomars, moonstruck, mrs.brown, mud, mulhollanddrive, 
#    mumford, mummythe, musicoftheheart, mybestfriendswedding, 
#    mymotherdreamsthesatansdisciplesinnewyork, mysterymen, 
#    naturalbornkillers, nextfriday, nickoftime, nightmareonelmstreeta, 
#    nightmareonelmstreetthefinalchapter, nottinghill, nursebetty, 
#    oneflewoverthecuckoosnest, onlygodforgives, outofsight, 
#    panicroom, patriotthe, pearlharbor, peepingtom, peggysuegotmarried, 
#    petsematary, pi, pianistthe, pitchblack, platoon, pleasantville, 
#    pointbreak, prettywoman, prettywomanfinalscript, programthe, 
#    prometheus, prophecythe, psycho, pulpfiction, punchdrunklove, 
#    quantumproject, raisingarizona, ramblingrose, rambofirstbloodiithemission, 
#    rearwindow, reindeergames, relicthe, residentevil, 
#    ringu, riseoftheguardians, rko281, rockthe, rocky, 
#    ronin, runawaybride, rushhour, rushhour2, saltonseathe, 
#    sandlotkidsthe, savingmr.banks, savingprivateryan, 
#    scream, scream2, se7en, searchersthe, senseandsensibility, 
#    sexliesandvideotape, shakespeareinlove, shampoo, shawshankredemptionthe, 
#    shivers, shrekthethird, sideways, signs, silenceofthelambs, 
#    silverbullet, sisteract, sixthsensethe, sleepyhollow, 
#    slingblade, snowfallingoncedars, soimarriedanaxemurderer, 
#    solaris, soldier, someonetowatchoverme, spareme, sphere, 
#    spiderman, starman, starshiptroopers, startrekiithewrathofkhan, 
#    startreknemesis, starwarsanewhope, starwarsattackoftheclones, 
#    starwarstheempirestrikesback, stateandmain, stationwest, 
#    stepmom, stingthe, storytelling, strangedays, sugarandspice, 
#    super8, sweetsmellofsuccess, swingers, takeshelter, 
#    takingsides, talentedmr.ripleythe, taxidriver, ted, 
#    terminator, terminator2judgementday, thingthe, thirteendays, 
#    threekings, threekingsspoilsofwar, thunderheart, timemachinethe, 
#    tombstone, tomorrowneverdies, tosleepwithanger, totalrecall, 
#    toystory, traffic, tremors, truelies, underfire, upintheair, 
#    uturn, verdictthe, verybadthings, vforvendetta, walkingtall, 
#    wallstreet, wallstreetmoneyneversleeps, warhorse, waroftheworlds, 
#    warrior, whatliesbeneath, whenastrangercalls, whitesquall, 
#    whosyourdaddy, wildatheart, winwin, witness, wonderboys, 
#    worldisnotenoughthe, xfilesfightthefuturethe, xmen, 
#    xxx, youcancountonme, 

screenplaySelection <- screenplaySelectionDraft[
  dialogueIndentCount <= 3 & 
    characterIndentCount <= 2 & 
    descriptionIndentCount == 1
]

# Create subset of screenplayIndentationStats for selected screenplays.
screenplayIndentationStatsSelection <- screenplaySelection[,.(movie)][
  screenplayIndentationStats
  , on = .(movie)
  , nomatch = FALSE
]

# Create subset of screenplayStats for selected screenplays.
screenplayStatsSelection <- screenplaySelection[,.(movie)][
  screenplayStats
  , on = .(movie)
  , nomatch = FALSE
]

###############################################################################
# Export results
data.table::fwrite(
  screenplayPaths
  , file.path(folder.data.processed, '301_screenplayPaths.csv')
  , quote = FALSE
  , row.names = FALSE
  , sep = '|'
)

data.table::fwrite(
  screenplaySelection
  , file.path(folder.data.processed, '302_screenplaySelection.csv')
  , quote = FALSE
  , row.names = FALSE
)

data.table::fwrite(
  screenplayIndentationStatsSelection
  , file.path(
      folder.data.processed,
      '303_screenplayIndentationStatsSelection.csv'
    )
  , quote = FALSE
  , row.names = FALSE
)

# Exporting as compressed file to avoid github's 100 MB per file limit.
saveRDS(
  screenplayStatsSelection
  , file.path(folder.data.processed, '304_screenplayStatsSelection.rds')
)
