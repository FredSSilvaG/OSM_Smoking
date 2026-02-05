
# color_palettes.R

bacterial_genus_colors <- c(
  "Cutibacterium" = "goldenrod",
  "Acinetobacter" = "dodgerblue",
  "Moraxella" = "royalblue",
  "GGB2722" = "khaki",
  "Gluconacetobacter" = "steelblue3",
  "Cloacibacterium" = "seagreen1",
  "Staphylococcus" = "tomato3",
  "Desulfovibrio" = "midnightblue",
  "Erythrobacter" = "cornflowerblue",
  "Oleisolibacter" = "steelblue4",
  "Tepidimonas" = "skyblue3",
  "Limosilactobacillus" = "orangered",
  "Corynebacterium" = "goldenrod3",
  "Malassezia" = "saddlebrown",
  "Deinococcus" = "midnightblue",
  "Lactobacillus" = "tomato1",
  "Listeria" = "firebrick",
  "Salmonella" = "steelblue2",
  "Escherichia" = "deepskyblue",
  "Enterococcus" = "indianred",
  "Xanthomonas" = "mediumblue",
  "Pseudomonas" = "slateblue",
  "GGB43920" = "yellow2",
  "Saccharomyces" = "seagreen3",
  "Acetobacter" = "deepskyblue3",
  "Cryptococcus" = "peru",
  "Micrococcus" = "khaki3",
  "Brevundimonas" = "cadetblue3",
  "Rhodovarius" = "darkslateblue",
  "GGB6646" = "forestgreen",
  "Caulobacter" = "skyblue",
  "Acidovorax" = "mediumslateblue",
  "Bradyrhizobium" = "greenyellow",
  "Sphingomonas" = "powderblue",
  "Lacticaseibacillus" = "lightcoral",
  "Lentilactobacillus" = "salmon",
  "Paucibacter" = "darkgreen",
  "Lactococcus" = "red3",
  "Sphingobium" = "mediumturquoise",
  "Azospira" = "darkcyan",
  "Burkholderia" = "royalblue",
  "Luteococcus" = "yellow",
  "Schleiferilactobacillus" = "orangered",
  "Methylobacterium" = "slategray",
  "GGB10485" = "springgreen",
  "Novosphingobium" = "turquoise",
  "Rhodoplanes" = "blueviolet",
  "Comamonas" = "darkturquoise",
  "GGB3493" = "green",
  "Streptococcus" = "darkorange",
  "Finegoldia" = "coral",
  "Peptoniphilus" = "tomato",
  "GGB2949" = "lightseagreen",
  "Anaerococcus" = "salmon",
  "Prevotella" = "mediumseagreen",
  "Winkia" = "sienna",
  "Chroococcidiopsis" = "purple",
  "Bacillus" = "lightpink",
  "Kocuria" = "gold",
  "Rhizobium" = "steelblue",
  "Kytococcus" = "lemonchiffon",
  "Neisseria" = "blue",
  "Salinicoccus" = "darkgoldenrod",
  "Haemophilus" = "slateblue",
  "Microbacterium" = "darkkhaki",
  "Cupriavidus" = "darkolivegreen",
  "Methylobacillus" = "cornflowerblue",
  "Formosa" = "yellowgreen"
)



bacterial_phyla_colors <- c(
  "Actinobacteria" = "gold1",
  "Proteobacteria" = "steelblue1",
  "Bacteroidota" = "yellowgreen",
  "Firmicutes" = "tomato",
  "Basidiomycota" = "brown",
  "Deinococcus_Thermus" = "midnightblue",
  "Ascomycota" = "seagreen",
  "Cyanobacteria" = "purple"
)

bacterial_species_colors <- c(
  # Limosilactobacillus (pinkish reds)
  "Limosilactobacillus_fermentum" = "lightpink",
  "Staphylococcus_aureus" = "firebrick1",
  "Staphylococcus_epidermidis" = "mistyrose",
  "Listeria_monocytogenes" = "lightsalmon",
  "Moraxella_osloensis" = "hotpink",
  "Cutibacterium_acnes" = "seagreen",
  "Cutibacterium_granulosum" = "navajowhite",
  "Cutibacterium_namnetense" = "peachpuff",
  "Acinetobacter_junii" = "khaki4",
  "Acinetobacter_johnsonii" = "burlywood1",
  "Acinetobacter_schindleri" = "wheat1",
  "Acetobacter_aceti" = "darkkhaki",
  "Acetobacter_musti" = "wheat",
  "GGB2722_SGB3663" = "goldenrod4",
  "Cloacibacterium_caeni" = "darkolivegreen3",
  "Erythrobacter_cryptus" = "darkolivegreen",
  "Sphingobium_yanoikuyae" = "sandybrown",
  "Novosphingobium_percolationis" = "olivedrab1",
  "Sphingomonas_ursincola" = "yellowgreen",
  "Brevundimonas_aurantiaca" = "mediumseagreen",
  "Corynebacterium_tuberculostearicum" = "mediumaquamarine",
  "Corynebacterium_SGB17002" = "mediumseagreen",
  "Corynebacterium_glucuronolyticum" = "palegreen",
  "Corynebacterium_aurimucosum" = "lightgreen",
  "Corynebacterium_sp_ACRPR" = "mediumspringgreen",
  "Corynebacterium_bovis" = "palegreen2",
  "Corynebacterium_macginleyi" = "palegreen3",
  "Corynebacterium_mastitidis" = "springgreen",
  "Corynebacterium_sp_ACRQP" = "darkseagreen",
  "Corynebacterium_hadale" = "palegreen1",
  "Corynebacterium_propinquum" = "mediumseagreen",
  "Deinococcus_geothermalis" = "mediumturquoise",
  "Microbacterium_arborescens" = "lightseagreen",
  "Cupriavidus_gilardii" = "darkturquoise",
  "Rhizobiales_bacterium_CCH10_E5" = "turquoise3",
  "Xanthomonas_campestris" = "turquoise4",
  "Rothia_SGB49305" = "darkcyan",
  "Lactobacillus_crispatus" = "cadetblue",
  "Lactobacillus_jensenii" = "deepskyblue",
  "Haemophilus_parainfluenzae" = "skyblue3",
  "GGB6646_SGB9384" = "deepskyblue3",
  "Comamonas_aquatica" = "lightskyblue3",
  "Caulobacter_sp_3R27C2_B" = "lightsteelblue3",
  "Rhodovarius_crocodyli" = "lightsteelblue2",
  "Gluconacetobacter_dulcium" = "thistle",
  "Oleisolibacter_albus" = "thistle2",
  "Azospira_inquinata" = "plum3",
  "Desulfovibrio_desulfuricans" = "plum2",
  "Chroococcidiopsis_cubana" = "orchid3",
  "Diaphorobacter_nitroreducens" = "orchid2",
  "Acidovorax_temperans" = "violetred1",
  "Kocuria_palustris" = "orchid",
  "Kytococcus_sedentarius" = "palevioletred1",
  "Streptococcus_sanguinis" = "hotpink3"
)


fungi_genus_colors <- c(
  "Sordaria" = "gold",
  "Saccharomyces" = "chocolate",
  "Malassezia" = "lightblue",
  "Debaryomyces" = "lightgreen",
  "Cryptococcus neoformans species complex" = "firebrick")

fungi_family_colors <- c(
  "Sordariaceae" = "goldenrod",
  "Saccharomycetaceae" = "chocolate",
  "Malasseziaceae" = "lightblue",
  "Debaryomycetaceae" = "lightgreen",
  "Cryptococcaceae" = "limegreen")

fungi_phyla_colors <- c(
  "Basidiomycota" = "brown",
  "Ascomycota" = "seagreen")

fungi_species_colors <- c(
  "Sordaria macrospora" = "goldenrod",
  "Saccharomyces cerevisiae"= "lightblue",
  "Malassezia sympodialis"  = "sienna",
  "Malassezia globosa" = "thistle3",
  "Debaryomycetes hansenii"= "limegreen",
  "Cryptococcus neoformans"= "orangered")


viral_phyla_colors <- c(
  "Retro-transcribing viruses" = "brown",
  "dsDNA viruses, no RNA stage" = "seagreen")

# Unique species names
viral_species_colors <- c(
  "Human betaherpesvirus 6A" = "darkred",
  "Human betaherpesvirus 7" = "firebrick",
  "Alphapapillomavirus 4" = "goldenrod",
  "Human betaherpesvirus 6B" = "tomato",
  "Lactobacillus phage PL-1" = "dodgerblue",
  "Cotesia congregata bracovirus" = "orchid",
  "Listeria phage A118" = "steelblue",
  "Listeria phage A500" = "royalblue",
  "Lactobacillus phage A2" = "skyblue",
  "BeAn 58058 virus" = "slateblue",
  "Staphylococcus prophage phiPV83" = "mediumpurple",
  "Alcelaphine gammaherpesvirus 2" = "red3",
  "Lactobacillus phage Lc-Nu" = "turquoise4",
  "Cyprinid herpesvirus 3" = "darkorange",
  "Pseudomonas phage EL" = "chartreuse4",
  "Lactobacillus phage phiAT3" = "deepskyblue4",
  "Cyprinid herpesvirus 1" = "orangered",
  "Listeria phage A006" = "lightskyblue",
  "Human papillomavirus type 10" = "tan3",
  "Pseudomonas phage F10" = "green3",
  "Staphylococcus virus 108PVL" = "chocolate4",
  "Glypta fumiferanae ichnovirus" = "plum4",
  "Staphylococcus phage tp310-2" = "royalblue4",
  "Staphylococcus phage tp310-3" = "gray40",
  "Merkel cell polyomavirus" = "palevioletred3",
  "Lactobacillus phage Lrm1" = "lightsteelblue",
  "Stx2-converting phage 1717" = "rosybrown3",
  "Lactobacillus virus Lb338-1" = "skyblue4",
  "Emiliania huxleyi virus 86" = "seagreen3",
  "Ictalurid herpesvirus 1" = "coral3",
  "Salmonella phage RE-2010" = "salmon4",
  "Enterobacteria phage IME10" = "darkgoldenrod3",
  "Staphylococcus phage StB20" = "steelblue4",
  "Staphylococcus phage StB27" = "mediumpurple4",
  "Salmonella phage SPN3UB" = "coral2",
  "Salmonella phage SSU5" = "indianred",
  "uncultured crAssphage" = "darkolivegreen3",
  "Pseudomonas phage JBD25" = "cadetblue4",
  "Staphylococcus phage StauST398-4" = "lightcoral",
  "Pandoravirus dulcis" = "orchid4",
  "Pandoravirus salinus" = "darkorchid",
  "Lactobacillus phage J-1" = "lightblue4",
  "Listeria phage LP-030-3" = "steelblue3",
  "Pseudomonas phage KPP25" = "mediumseagreen",
  "Enterobacteria phage P88" = "burlywood4",
  "Staphylococcus phage phiIPLA-C1C" = "brown3",
  "Listeria phage vB_LmoS_293" = "cornflowerblue",
  "Salmonella phage SEN34" = "firebrick4",
  "Staphylococcus phage SPbeta-like" = "darkslategray",
  "Lactobacillus phage iLp84" = "slategray4",
  "Lactobacillus phage iLp1308" = "dodgerblue4",
  "Pseudomonas phage YMC11/02/R656" = "aquamarine4",
  "Lactobacillus phage PLE3" = "lightslateblue"
)


viral_genus_colors <- c(
  "unclassified Spbetalikevirus" = "gold",
  "Unclassified" = "chocolate",
  "Elvirus" = "lightblue",
  "Biseptimavirus" = "royalblue",
  "Debaryomyces" = "lightgreen",
  "Alphapapillomavirus" = "firebrick")

viral_family_colors <- c(
  "unclassified Chordopoxvirinae" = "goldenrod",
  "Unclassified" = "chocolate")

viral_order_colors <- c(
  "Herpesvirales" = "darkred",
  "Unclassified" = "skyblue4",
  "Caudovirales" = "chartreuse4",
  "unclassified bacterial viruses" = "orange",
  "unclassified dsDNA viruses" = "purple"
)
