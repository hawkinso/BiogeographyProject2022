# Biogeopgraphy Project 2022 
# Author(s): Olivia H. Hawkins 
# Date: 02/10/2022 
# Goals: Extract data from FishBase 

# Git-hub 
browseURL("https://github.com/hawkinso/BiogeographyProject2022.git")

# Load libraries 
library(rfishbase)
library(tidyverse)
library(tidyr)
library(ape)
library(ggplot2)
library(ggtree)
library(Biostrings)

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("ggtree")

# Upload species specific data 
data <- read.csv("Species_Author_CommonName_BiogeographyProject.csv")

# Convert data to a list 
species.names <- as.list(data$Scientific.Name)

# Make a vector for species names 
labrids <- c("Acantholabrus palloni", "Achoerodus gouldii", "Achoerodus viridis", "Ammolabrus dicrus",
              "Anampses caeruleopunctatus", "Anampses chrysocephalus", "Anampses cuvier", 
              "Anampses elegans", "Anampses femininus", "Anampses geographicus", "Anampses lennardi", 
              "Anampses lineatus", "Anampses melanurus", "Anampses meleagrides", 
              "Anampses neoguinaicus", "Anampses twistii", "Anchichoerops natalensis", 
              "Austrolabrus maculatus", "Bodianus axillaris", "Bodianus bathycapros", 
              "Bodianus bennetti", "Bodianus bilunulatus", "Bodianus bimaculatus", 
              "Bodianus busellatus", "Bodianus cylindriatus", "Bodianus diana", 
              "Bodianus dictynna", "Bodianus diplotaenia", "Bodianus eclancheri", 
              "Bodianus flavifrons", "Bodianus flavipinnis", "Bodianus frenchii", 
              "Bodianus albotaeniatus", "Bodianus anthioides", "Bodianus atrolumbus", 
              "Bodianus insularis", "Bodianus izuensis", "Bodianus leucosticticus", 
              "Bodianus loxozonus", "Bodianus macrognathos", "Bodianus macrourus", 
              "Bodianus masudai", "Bodianus mesothorax", "Bodianus neilli", "Bodianus neopercularis", 
              "Bodianus opercularis", "Bodianus oxycephalus", "Bodianus paraleucosticticus", 
              "Bodianus perditio", "Bodianus prognathus", "Bodianus pulchellus", "Bodianus rubrisos", 
              "Bodianus rufus", "Bodianus sanguineus", "Bodianus scrofa", "Bodianus sepiacaudus", 
              "Bodianus solatus", "Bodianus speciosus", "Bodianus tanyokidus", "Bodianus thoracotaeniatus", 
              "Bodianus trilineatus", "Bodianus unimaculatus", "Bodianus vulpinus", "Centrolabrus exoletus", 
              "Centrolabrus melanocercus", "Cheilinus abudjubbe", "Cheilinus chlorourus", "Cheilinus fasciatus", 
              "Cheilinus lunulatus", "Cheilinus oxycephalus", "Cheilinus quinquecinctus", "Cheilinus trilobatus", 
              "Cheilinus undulatus", "Cheilio inermis", "Choerodon anchorago", "Choerodon aurulentus", 
              "Choerodon azurio", "Choerodon cauteroma", "Choerodon cephalotes", "Choerodon cyanodus", 
              "Choerodon cypselurus", "Choerodon fasciatus", "Choerodon frenatus", "Choerodon gomoni", 
              "Choerodon graphicus", "Choerodon gymnogenys", "Choerodon japonicus", "Choerodon jordani", 
              "Choerodon margaritiferus", "Choerodon melanostigma", "Choerodon monostigma", 
              "Choerodon oligacanthus", "Choerodon paynei", "Choerodon robustus", "Choerodon rubescens", 
              "Choerodon schoenleinii", "Choerodon skaiopygmaeus", "Choerodon sugillatum", "Choerodon venustus", 
              "Choerodon vitta", "Choerodon zamboangae", "Choerodon zosterophorus", "Cirrhilabrus adornatus", 
              "Cirrhilabrus africanus", "Cirrhilabrus aurantidorsalis", "Cirrhilabrus balteatus", 
              "Cirrhilabrus bathyphilus", "Cirrhilabrus beauperryi", "Cirrhilabrus blatteus", 
              "Cirrhilabrus briangreenei", "Cirrhilabrus brunneus", "Cirrhilabrus cenderawasih", 
              "Cirrhilabrus claire", "Cirrhilabrus condei", "Cirrhilabrus cyanogularis", "Cirrhilabrus cyanopleura", 
              "Cirrhilabrus earlei", "Cirrhilabrus efatensis", "Cirrhilabrus exquisitus", "Cirrhilabrus filamentosus", 
              "Cirrhilabrus flavidorsalis", "Cirrhilabrus greeni", "Cirrhilabrus humanni", "Cirrhilabrus hygroxerus", 
              "Cirrhilabrus isosceles", "Cirrhilabrus joanallenae", "Cirrhilabrus johnsoni", "Cirrhilabrus jordani", 
              "Cirrhilabrus katherinae", "Cirrhilabrus katoi", "Cirrhilabrus laboutei", "Cirrhilabrus lanceolatus", 
              "Cirrhilabrus lineatus", "Cirrhilabrus lubbocki", "Cirrhilabrus lunatus", "Cirrhilabrus luteovittatus", 
              "Cirrhilabrus marinda", "Cirrhilabrus marjorie", "Cirrhilabrus melanomarginatus", "Cirrhilabrus morrisoni", 
              "Cirrhilabrus nahackyi", "Cirrhilabrus naokoae", "Cirrhilabrus punctatus", "Cirrhilabrus pylei", "Cirrhilabrus randalli", 
              "Cirrhilabrus rhomboidalis", "Cirrhilabrus roseafascia", "Cirrhilabrus rubeus", "Cirrhilabrus rubrimarginatus", 
              "Cirrhilabrus rubripinnis", "Cirrhilabrus rubrisquamis", "Cirrhilabrus rubriventralis", "Cirrhilabrus ryukyuensis", 
              "Cirrhilabrus sanguineus", "Cirrhilabrus scottorum", "Cirrhilabrus shutmani", "Cirrhilabrus solorensis", "Cirrhilabrus squirei",
              "Cirrhilabrus temminckii", "Cirrhilabrus tonozukai", "Cirrhilabrus wakanda", "Cirrhilabrus walindi", "Cirrhilabrus walshi", 
              "Clepticus africanus", "Clepticus brasiliensis", "Clepticus parrae", "Coris caudimacula", "Coris centralis", "Coris cuvieri", 
              "Coris debueni", "Coris dorsomacula", "Coris flavovittata", "Conniella apterygia", "Coris atlantica", "Coris auricularis", 
              "Coris aurilineata", "Coris aygula", "Coris ballieui", "Coris batuensis", "Coris bulbifrons", "Coris formosa", "Coris gaimard",
              "Coris hewetti", "Coris julis", "Coris latifasciata", "Coris marquesensis", "Coris musume", "Coris nigrotaenia", "Coris picta", 
              "Coris pictoides", "Coris roseoviridis", "Coris sandeyeri", "Coris variegata", "Coris venusta", "Ctenolabrus rupestris", 
              "Cymolutes lecluse", "Cymolutes praetextatus", "Cymolutes torquatus", "Decodon grandisquamis", "Decodon melasma", 
              "Decodon pacificus", "Decodon puellaris", "Diproctacanthus xanthurus", "Doratonotus megalepis", "Dotalabrus alleni", 
              "Dotalabrus aurantiacus", "Epibulus brevis", "Epibulus insidiator", "Eupetrichthys angustipes", "Frontilabrus caeruleus", 
              "Gomphosus caeruleus", "Gomphosus varius", "Halichoeres adustus", "Halichoeres aestuaricola", "Halichoeres argus", 
              "Halichoeres bathyphilus", "Halichoeres bicolor", "Halichoeres binotopsis", "Halichoeres biocellatus", "Halichoeres bivittatus",
              "Halichoeres bleekeri", "Halichoeres brasiliensis", "Halichoeres brownfieldi", "Halichoeres burekae", "Halichoeres caudalis", 
              "Halichoeres chierchiae", "Halichoeres chlorocephalus", "Halichoeres chloropterus", "Halichoeres chrysotaenia", "Halichoeres chrysus", 
              "Halichoeres claudia", "Halichoeres cosmetus", "Halichoeres cyanocephalus", "Halichoeres dimidiatus", "Halichoeres discolor", 
              "Halichoeres dispilus", "Halichoeres erdmanni", "Halichoeres garnoti", "Halichoeres gurrobyi", "Halichoeres hartzfeldii", 
              "Halichoeres hilomeni", "Halichoeres hortulanus", "Halichoeres inornatus", "Halichoeres insularis", "Halichoeres iridis", 
              "Halichoeres kallochroma", "Halichoeres kneri", "Halichoeres lapillus", "Halichoeres leptotaenia", "Halichoeres leucoxanthus", 
              "Halichoeres leucurus", "Halichoeres maculipinna", "Halichoeres malpelo", "Halichoeres margaritaceus", "Halichoeres marginatus", 
              "Halichoeres melanochir", "Halichoeres melanotis", "Halichoeres melanurus", "Halichoeres melas", "Halichoeres melasmapomus",
              "Halichoeres miniatus", "Halichoeres nebulosus", "Halichoeres nicholsi", "Halichoeres nigrescens", "Halichoeres notospilus", 
              "Halichoeres orientalis", "Halichoeres ornatissimus", "Halichoeres pallidus", "Halichoeres papilionaceus", 
              "Halichoeres pardaleocephalus", "Halichoeres pelicieri", "Halichoeres penrosei", "Halichoeres pictus", 
              "Halichoeres podostigma", "Halichoeres poeyi", "Halichoeres prosopeion", "Halichoeres radiatus", "Halichoeres richmondi", 
              "Halichoeres rubricephalus", "Halichoeres rubrovirens", "Halichoeres salmofasciatus", "Halichoeres sazimai", 
              "Halichoeres scapularis", "Halichoeres semicinctus", "Halichoeres signifer", "Halichoeres socialis", "Halichoeres solorensis",
              "Halichoeres stigmaticus", "Halichoeres tenuispinis", "Halichoeres timorensis", "Halichoeres trimaculatus", 
              "Halichoeres trispilus", "Halichoeres zeylonicus", "Halichoeres zulu", "Hemigymnus fasciatus", "Hemigymnus melapterus", 
              "Hemigymnus sexfasciatus", "Hologymnosus annulatus", "Hologymnosus doliatus", "Hologymnosus longipes", "Hologymnosus rhodonotus", 
              "Iniistius aneitensis", "Iniistius auropunctatus", "Iniistius baldwini", "Iniistius bimaculatus", "Iniistius brevipinnis", 
              "Iniistius celebicus", "Iniistius cyanifrons", "Iniistius dea", "Iniistius evides", "Iniistius geisha", "Iniistius griffithsi", 
              "Iniistius jacksonensis", "Iniistius melanopus", "Iniistius naevus", "Iniistius pavo", "Iniistius pentadactylus", 
              "Iniistius spilonotus", "Iniistius trivittatus", "Iniistius twistii", "Iniistius umbrilatus", "Iniistius verrens", 
              "Lappanella fasciata", "Lappanella guineensis", "Larabicus quadrilineatus", "Labrichthys unilineatus", "Labroides bicolor", 
              "Labroides dimidiatus", "Labroides pectoralis", "Labroides phthirophagus", "Labroides rubrolabiatus", "Labropsis alleni", 
              "Labropsis australis", "Labropsis manabei", "Labropsis micronesica", "Labropsis polynesica", "Labropsis xanthonota", 
              "Labrus bergylta", "Labrus merula", "Labrus mixtus", "Labrus viridis", "Lachnolaimus maximus", "Leptojulis chrysotaenia", 
              "Leptojulis cyanopleura", "Leptojulis lambdastigma", "Leptojulis polylepis", "Leptojulis urostigma", 
              "Macropharyngodon bipartitus", "Macropharyngodon choati", "Macropharyngodon cyanoguttatus", "Macropharyngodon geoffroy", 
              "Macropharyngodon kuiteri", "Macropharyngodon marisrubri", "Macropharyngodon meleagris", "Macropharyngodon moyeri", 
              "Macropharyngodon negrosensis", "Macropharyngodon ornatus", "Macropharyngodon pakoko", "Macropharyngodon vivienae", 
              "Malapterus reticulatus", "Minilabrus striatus", "Notolabrus celidotus", "Notolabrus cinctus", "Notolabrus fucicola", 
              "Notolabrus gymnogenis", "Notolabrus inscriptus", "Notolabrus parilus", "Notolabrus tetricus", "Novaculichthys taeniourus", 
              "Novaculoides macrolepidotus", "Novaculops alvheimi", "Novaculops compressus", "Novaculops halsteadi", "Novaculops koteamea",
              "Novaculops pastellus", "Novaculops sciistius", "Novaculops woodi", "Ophthalmolepis lineolata", "Oxycheilinus arenatus", 
              "Oxycheilinus bimaculatus", "Oxycheilinus celebicus", "Oxycheilinus digramma", "Oxycheilinus lineatus", "Oxycheilinus mentalis",
              "Oxycheilinus nigromarginatus", "Oxycheilinus orientalis", "Oxycheilinus samurai", "Oxycheilinus unifasciatus", "Oxyjulis californica", 
              "Paracheilinus alfiani", "Paracheilinus angulatus", "Paracheilinus attenuatus", "Paracheilinus bellae", "Paracheilinus carpenteri", 
              "Paracheilinus cyaneus", "Paracheilinus filamentosus", "Paracheilinus flavianalis", "Paracheilinus hemitaeniatus", 
              "Paracheilinus lineopunctatus", "Paracheilinus mccoskeri", "Paracheilinus nursalim", "Paracheilinus octotaenia", 
              "Paracheilinus paineorum", "Paracheilinus piscilineatus", "Paracheilinus rennyae", "Paracheilinus rubricaudalis", 
              "Paracheilinus togeanensis", "Paracheilinus walton", "Parajulis poecilepterus", "Pictilabrus brauni", "Pictilabrus laticlavius",
              "Pictilabrus viridis", "Polylepion cruentum", "Polylepion russelli", "Pseudocheilinops ataenia", "Pseudocheilinus citrinus", 
              "Pseudocheilinus dispilus", "Pseudocheilinus evanidus", "Pseudocheilinus hexataenia", "Pseudocheilinus ocellatus",
              "Pseudocheilinus octotaenia", "Pseudocheilinus tetrataenia", "Pseudocoris aequalis", "Pseudocoris aurantiofasciata", 
              "Pseudocoris bleekeri", "Pseudocoris hemichrysos", "Pseudocoris heteroptera", "Pseudocoris occidentalis", "Pseudocoris ocellata", 
              "Pseudocoris petila", "Pseudocoris yamashiroi", "Pseudodax moluccanus", "Pseudogoniistius nigripes", "Pseudojuloides argyreogaster", 
              "Pseudojuloides atavai", "Pseudojuloides cerasinus", "Pseudojuloides crux", "Pseudojuloides edwardi", "Pseudojuloides elongatus", 
              "Pseudojuloides erythrops", "Pseudojuloides kaleidos", "Pseudojuloides labyrinthus", "Pseudojuloides mesostigma", 
              "Pseudojuloides paradiseus", "Pseudojuloides pluto", "Pseudojuloides polackorum", "Pseudojuloides polynesica", 
              "Pseudojuloides proserpina", "Pseudojuloides pyrius", "Pseudojuloides severnsi", "Pseudojuloides splendens", 
              "Pseudojuloides xanthomos", "Pseudojuloides zeus", "Pseudolabrus biserialis", "Pseudolabrus eoethinus", "Pseudolabrus fuentesi", 
              "Pseudolabrus gayi", "Pseudolabrus guentheri", "Pseudolabrus japonicus", "Pseudolabrus luculentus", "Pseudolabrus miles", 
              "Pseudolabrus rubicundus", "Pseudolabrus semifasciatus", "Pseudolabrus sieboldi", "Pseudolabrus torotai", "Pteragogus aurigarius",
              "Pteragogus clarkae", "Pteragogus cryptus", "Pteragogus enneacanthus", "Pteragogus flagellifer", "Pteragogus guttatus", 
              "Pteragogus pelycus", "Pteragogus taeniops", "Pteragogus trispilus", "Pteragogus variabilis", "Semicossyphus darwini", 
              "Semicossyphus pulcher", "Semicossyphus reticulatus", "Stethojulis albovittata", "Stethojulis balteata", "Stethojulis bandanensis", 
              "Stethojulis interrupta", "Stethojulis maculata", "Stethojulis marquesensis", "Stethojulis notialis", "Stethojulis strigiventer", 
              "Stethojulis terina", "Stethojulis trilineata", "Suezichthys arquatus", "Suezichthys aylingi", "Suezichthys bifurcatus", 
              "Suezichthys caudavittatus", "Suezichthys cyanolaemus", "Suezichthys devisi", "Suezichthys gracilis", "Suezichthys notatus", 
              "Suezichthys ornatus", "Suezichthys rosenblatti", "Suezichthys russelli", "Suezichthys soelae", "Symphodus bailloni", 
              "Symphodus caeruleus", "Symphodus cinereus", "Symphodus doderleini", "Symphodus mediterraneus", "Symphodus melops", 
              "Symphodus ocellatus", "Symphodus roissali", "Symphodus rostratus", "Symphodus tinca", "Symphodus trutta", "Terelabrus dewapyle", 
              "Terelabrus rubrovittatus", "Tautoga onitis", "Tautogolabrus adspersus", "Thalassoma amblycephalum", "Thalassoma ascensionis", 
              "Thalassoma ballieui", "Thalassoma bifasciatum", "Thalassoma cupido", "Thalassoma duperrey", "Thalassoma genivittatum", 
              "Thalassoma grammaticum", "Thalassoma hardwicke", "Thalassoma hebraicum", "Thalassoma heiseri", "Thalassoma jansenii", 
              "Thalassoma loxum", "Thalassoma lucasanum", "Thalassoma lunare", "Thalassoma lutescens", "Thalassoma newtoni", 
              "Thalassoma nigrofasciatum", "Thalassoma noronhanum", "Thalassoma pavo", "Thalassoma purpureum", "Thalassoma quinquevittatum", 
              "Thalassoma robertsoni", "Thalassoma rueppellii", "Thalassoma sanctaehelenae", "Thalassoma septemfasciatum", 
              "Thalassoma trilobatum", "Thalassoma virens", "Wetmorella albofasciata", "Wetmorella nigropinnata", "Wetmorella tanakai", 
              "Xenojulis margaritacea", "Xiphocheilus typus", "Xyrichtys blanchardi", "Xyrichtys incandescens", "Xyrichtys javanicus", 
              "Xyrichtys martinicensis", "Xyrichtys mundiceps", "Xyrichtys novacula", "Xyrichtys rajagopalani", "Xyrichtys sanctaehelenae", 
              "Xyrichtys splendens", "Xyrichtys victori", "Xyrichtys wellingtoni")

# Get common names 
common.names <- common_names(
  species_list = labrids,
  server = getOption("FISHBASE_API", "fishbase"),
  Language = "English",
  fields = NULL)

# Get country 
Country <- country(
  species_list = labrids,
  server = getOption("FISHBASE_API", "fishbase"),
  Language = "English",
  fields = NULL)

Country <- Country %>%
  select(Species,Status,CurrentPresence,country) 

# Get distribution 
Distribution <- distribution(
  species_list = labrids,
  server = getOption("FISHBASE_API", "fishbase"),
  Language = "English",
  fields = NULL)

Distribution <- Distribution %>% 
  select(Species,Status,Entered,FAO,LatDeg,LongDeg,N_S,E_W,NorthernLatitude,NorthernLatitudeNS,SouthernLatitude,
         SouthernLatitudeNS,EasternLongitude,EasternLongitudeEW,WesternLongitude,WesternLongitudeEW)

# Get ecology 
Ecology <- ecology(
  species_list = labrids,
  server = getOption("FISHBASE_API", "fishbase"),
  Language = "English",
  fields = NULL)

Ecology <- Ecology %>% 
  select(Species,AddRems)

# Get ecosystem
Ecosystem <- ecosystem(
  species_list = labrids,
  server = getOption("FISHBASE_API", "fishbase"),
  Language = "English",
  fields = NULL)

Ecosystem <- Ecosystem %>%
  select(Species,EcosystemName,Climate,NorthernLat,NrangeNS,SouthernLat,SrangeNS,WesternLat,WrangeEW,
         EasternLat,ErangeEW,TotalCount,LatDegFill,LatMinFill,LongDegFill,LongMinFill,NorthSouthFill,EastWestFill)

# Get morphology data 
Morphology <- morphology(
  species_list = labrids,
  server = getOption("FISHBASE_API", "fishbase"),
  Language = "English",
  fields = NULL)

Morphology <- Morphology %>% 
  select(Species,BodyShapeI,BodyShapeII,StandardLengthCm,MaximumDepth)

# Get morphometrics data 
Morphometrics <- morphometrics(
  species_list = labrids,
  server = getOption("FISHBASE_API", "fishbase"),
  Language = "English",
  fields = NULL)

Morphometrics <- Morphometrics %>%
  select(Species,SL,BD,AspectRatio)


# PHYLOGENIES ---- 

# Import phylogeny
tree <- read.tree("Labridae_newick")

# Work with tree 
ggtree(tree,mrsd = TRUE)

