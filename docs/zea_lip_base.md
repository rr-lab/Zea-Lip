*Zea Lip*: An atlas of glycerolipid species across development in maize B73, CML312 inbreds and Palomero Toluqueño landrace.
------------------------


*Karla Juárez-Núñez, Guillaume Lobet,  Rubén Rellán-Álvarez*


# Manuscript Outline  

https://docs.google.com/document/d/1DJSuOG00NRG-zFrtNSyxSxVWbd4kW9SGacIzzcvHCLs/edit

### Introduction

### Results

- Figure 1. Tissues analyzed + table. Text analyzing general patterns of the data. We still need to check if all compounds are present in all tissues/genotypes.  

- Figure 2. PCA analysis. Show boxplots of most important variables. 
Tables of 10 most importan variables to PC1 and PC2.  

- Figure 3. Summary data by groups (PCs, DGs etc), ratios. 

- Figure 4. Heatmaps correlations. Compare correlations between genotypes.

- Figure 5. Expression data correlations.

## Abstract

Glycerolipids are the predominant building blocks of plant membranes, they are essential for plant growth and development.
They are involved in a variety of signalling and their relative abundances can change in response to several environmental factors.
In an effort to catalog the composition of glycerolipids in an important crop like maize, we used ultra high performance liquid chromatography coupled with quadrupole time of flight mass spectrometry to characterize the glycerolipid profile of samples of maize plants collected at 10 different vegetative developmental stages and 6 different leaf ages. 
We analyzed three different genotypes: B73, a temperate inbred, CML312, a tropical inbred and Palomero Toluqueño, an open pollinated variety landrace from the Mexican highlands. From plants grown in greenhouse, 150 samples were analyzed and from a highland field, another extra 24 samples, of a single developmental stage from the three genotypes, were analyzed.
Overall, genotype was the major driver of glycerolipid differences. Phosphatidylcholine and lyso-phosphatidylcholine genotypic differences were particularly high. We designed a web interface to easily browse and compare glycerlolipid levels across tissues and genotypes.    

**Keywords**
Maize, glycerolipids, natural variation, developmental stages,ultra high performance liquid chromatography, quadrupole time of flight mass spectrometry

## Introduction

In *Zea mays*, and monocots in general, the leaf differentiation occurs basipetally, having the less differentiated cells  in the tip and the most differentiated ones in the base (Sharman 1942; Nelson and Dengler 1997). This continuous developmental gradient represents a good model for different developmental studies. 

In previous studies of maize global RNA sequencing it has been reported that there is a very contrasting gene expression pattern between tip and base of maize leaves in same or different developmental stages (Sekhon et al. 2011). It has been reported that in unexpanded leaves, the base exhibits an exclusive proliferative, symmetric cell division (Facette et al, 2013). Moreover, Li and collaborators found that B73 maize third leaf blade represents the full immature to mature transition of variation in morphological, anatomical and physiological characteristics. According to that, the leaf blade can be divided into four zones, which are representative samples for the study of photosynthetic differentiation in maize leaves (Li et al. 2010). 
Based on those two different approaches we defined three different developmental zones within maize leaves (Figure 1): tip, center and base.

Here, we present a comparative lipid analysis across 3 maize genotypes (B73, CML312, PT), 10 different plant developmental stages (V4 - V14), 6 different leaf ages (leaf 4, 5, 7, 9, 11, 13) and 3 different developmental zones within the leaf (tip, center, base); all this from plants grown in greenhouse. We also analyzed these three genotypes grown in a highland conditions field, where we sampled leaf tissue around V3 - V6 developmental stage.  Using a Ultra High Performance Liquid Chromatography coupled to a Quadrupole Time of Flight Mass Spectrometry (UHPLC-QTOF MS/MS) on an ESI positive mode we were able to identify 171 molecular lipid species, within 14 lipid classes. 
This study provides an insight in the distribution of lipids through maize leaf development. 

 




## Materials and Methods  

### Plant material and growth conditions

The plant material used in this study consisted of Mexi 5 Palomero Toluqueño (mexican highland landrace), B73, a temperate inbred,  and CML312, a tropical line developed by CIMMYT. All seeds were weighed before planting (data not shown). These varieties of maize were planted in a greenhouse, in four different blocks, using a randomized block design. Each block was planted with 10 days of difference in between sowing dates. Within each block, two replicates of each genotype were included, the plants were watered with 500 mL of tap water every third day.
After germination, the growth was daily measured in terms of number of emerging leaves and fully expanded leaves by plant (Figure 2). The vegetative stage used describes the number of leaf, counting the coleoptiles as the first one (Sharman 1942).  
All blocks were harvested 58 days after planting (dap) of block 1, harvested tissue was immediately frozen in liquid nitrogen and stored at -80°C until lipid extraction.  
From block 1, leaves were sampled in three different zones: tip (T), center (C) and base (B), see Figure 1. From each zone 50 mg of fresh blade tissue were collected for lipid extraction, excluding the mid rib, the ligule and the auricles. A list of all tissues sampled is listed in Table 1.

![Figure1](https://github.com/rr-lab/zea_lip/blob/master/figures/Figure%201.png)
**Figure1. B73, CML312 and PT plants acrossed a developmental gradient.**

This image corresponds to the 4th leaf of a B73 maize plant, 28 days after planting, grown in a greenhouse.
We considered three developmental zones in the leaves to sample for lipid profiling, the base, the center and the tip.
For the base sampling, we took 10 punches corresponding to around 50 mgof fresh tissue, along 5 cm distance, starting 1 cm above from the auricles and ligule to exclude them. 
The tip was sampled from 2 cm below the tip, along a 5 cm distance, this to avoid the very first small region for taking exact punches (circles) to take the same weight for all samples.
Finally, the center in short leaves was sampled considering a separation of 2 cm distance from the end of the base and the tip. For longer leaves distances of 10 and 20 cm in between base and tip to center were considered.
For all samples, midrib was also avoided. 

![Figure2](https://github.com/rr-lab/zea_lip/blob/master/figures/figure_2/3Genotypes_DevStages_AllCompounds_PLSDA.png)
**Figure2. PLSD analysis of lipid profiling data.

Here we can observe that the major source of lipid variation is explained by the genotype.
According to the loading of the PLS analysis, the lipid species that have a major effect explaining the variation on PC1 are shown in the table above.  

![Figure](../output/Growth_rate_fully_average_expanded_leaves_per_genotype.png)
**Figure. Growth rate of B73, CML312 and PT in greenhouse.**
The average of fully expanded leaves per genotype per day were used to make this graph.


**Table 1. Leaf tissue sampled for lipid analysis**

<!---
we need to add info about the samples that were taken along the leaf
-->

| Genotype | Sampled leaf | DAG | Developmental Stage | Replicates |
|----------|--------------|-----|---------------------|------------|
| B73      | 11           | 57  | V11                 | 3          |
| B73      | 11           | 57  | V12                 | 3          |
| B73      | 11           | 49  | V9                  | 2          |
| B73      | 13           | 57  | V11                 | 3          |
| B73      | 13           | 57  | V12                 | 3          |
| B73      | 13           | 49  | V9                  | 2          |
| B73      | 4            | 49  | V10                 | 1          |
| B73      | 4            | 27  | V4                  | 2          |
| B73      | 4            | 40  | V6                  | 2          |
| B73      | 4            | 49  | V9                  | 1          |
| B73      | 5            | 57  | V11                 | 1          |
| B73      | 5            | 57  | V12                 | 3          |
| B73      | 5            | 27  | V4                  | 2          |
| B73      | 5            | 40  | V6                  | 2          |
| B73      | 5            | 49  | V9                  | 2          |
| B73      | 7            | 57  | V11                 | 3          |
| B73      | 7            | 57  | V12                 | 3          |
| B73      | 7            | 40  | V6                  | 2          |
| B73      | 7            | 49  | V9                  | 2          |
| B73      | 9            | 57  | V11                 | 3          |
| B73      | 9            | 57  | V12                 | 3          |
| B73      | 9            | 49  | V9                  | 2          |
| CML312   | 11           | 49  | V10                 | 2          |
| CML312   | 11           | 57  | V11                 | 3          |
| CML312   | 11           | 57  | V14                 | 3          |
| CML312   | 13           | 49  | V10                 | 1          |
| CML312   | 13           | 57  | V11                 | 4          |
| CML312   | 13           | 57  | V14                 | 3          |
| CML312   | 4            | 49  | V10                 | 1          |
| CML312   | 4            | 49  | V11                 | 1          |
| CML312   | 4            | 26  | V5                  | 1          |
| CML312   | 4            | 26  | V6                  | 1          |
| CML312   | 4            | 40  | V8                  | 2          |
| CML312   | 5            | 49  | V10                 | 1          |
| CML312   | 5            | 49  | V11                 | 1          |
| CML312   | 5            | 57  | V14                 | 3          |
| CML312   | 5            | 26  | V5                  | 1          |
| CML312   | 5            | 26  | V6                  | 1          |
| CML312   | 5            | 40  | V8                  | 1          |
| CML312   | 7            | 49  | V10                 | 1          |
| CML312   | 7            | 57  | V11                 | 4          |
| CML312   | 7            | 57  | V14                 | 3          |
| CML312   | 7            | 26  | V5                  | 1          |
| CML312   | 7            | 26  | V6                  | 1          |
| CML312   | 7            | 40  | V8                  | 2          |
| CML312   | 9            | 49  | V10                 | 1          |
| CML312   | 9            | 57  | V11                 | 4          |
| CML312   | 9            | 57  | V14                 | 3          |
| CML312   | 9            | 40  | V8                  | 2          |
| PT       | 11           | 49  | V12                 | 1          |
| PT       | 11           | 57  | V14                 | 6          |
| PT       | 11           | 49  | V8                  | 1          |
| PT       | 13           | 57  | V14                 | 6          |
| PT       | 13           | 49  | V8                  | 1          |
| PT       | 4            | 49  | V12                 | 1          |
| PT       | 4            | 28  | V4                  | 1          |
| PT       | 4            | 27  | V5                  | 1          |
| PT       | 4            | 40  | V7                  | 1          |
| PT       | 4            | 49  | V8                  | 2          |
| PT       | 5            | 49  | V12                 | 1          |
| PT       | 5            | 28  | V4                  | 1          |
| PT       | 5            | 27  | V5                  | 1          |
| PT       | 5            | 40  | V7                  | 1          |
| PT       | 5            | 49  | V8                  | 2          |
| PT       | 7            | 49  | V12                 | 1          |
| PT       | 7            | 57  | V14                 | 6          |
| PT       | 7            | 27  | V5                  | 1          |
| PT       | 7            | 40  | V7                  | 1          |
| PT       | 7            | 49  | V8                  | 2          |
| PT       | 9            | 49  | V12                 | 1          |
| PT       | 9            | 57  | V14                 | 6          |
| PT       | 9            | 40  | V7                  | 1          |
| PT       | 9            | 49  | V8                  | 2          |

In the field, 8 plants per genotype were sampled, in a CIMMYT experimental field located in Metepec, Mexico State, (19°13'28.7"N 99°32'51.6"W) within the Trans-Mexican volcanic belt. The experimental field is a highland one, being placed at 2610 meters above sea level (masl), the range of average monthly temperatures along the year go from 5°C (41°F) to 21.5°C (70.7°F) with an average annual of 13.6°C (56.48°F). Its average annual precipitation is around 809mm (31.85in). Plants were planted in on April 5th, 2016 and were sampled for lipid analysis by June 9th 2016, when the plants were in average around the V5 developmental stage. Samples were taken from the tip of the S2 leaf (two leaves younger of the last leaf with a fully developed collar). Eight plants per genotype were sampled.  
The average GDD for Metepec was 5.52 and the total GDD (over the length of the growing season) was 364.25. On the other hand, during this period the average daily precipitation was 1.6 mm (0.06in) while the total precipitation was 96.5mm (3.79in).


### Lipid extraction  

In the field, leaf tissue was collected when the plants were in the V3 - V6 stage. While in the greenhouse, plants were sampled as shown in Table 1. We collected 50 mg of wet weight leaf tissue for lipidome profiling, as this amount was deemed appropriate for *Arabidopsis* (Degenkolbe et al. 2012). Frozen material was homogenized in a tissue grinder Retsch (Haan, Germany) during 40 seconds at 30 1/s. After grinding all tubes were stored in liquid nitrogen.

Lipid extraction used was reported by Matyash and collaborators (Matyash et al. 2008). First 225 μL of cold methanol (MeOH), previously prepared with a Quality Control (QC) mix, was added to each sample, always keeping MeOH on ice during the extraction. Each sample was vortexed for 10 seconds, keeping the rest of materials on ice. Then 750 μL of cold methyl tert-butyl ether (MTBE) were added, keeping MTBE on ice along all the process. Again, each sample had to be vortexed for 10 seconds, followed by shaking each for 6 minutes at 4°C in the orbital mixer. After that, 188 μL of LC/MS grade water at room temperature (RT) were added, vortexing all the samples for 20 seconds.

In order to separate the phases, all samples were centrifuged for 2 min at 14,000 rcf (12,300 rpm). The upper phase is the organic one and it was possible to remove around 700 μL of supernatant. The supernatant was split into two aliquots of 350 μL, one serving as back up and the other for preparation of pools. Finally, samples were dried using a speed vacuum concentration system.  

### Lipid Profiling

Dry samples were resuspended in 110 μL of MeOH-Toluene 90:10 (with CUDA 50 ng/mL). The rack of samples was vortexed at low speed for 20 s and then all tubes were sonicated at RT for 5 min. Aliquots of 50 µL per sample were transferred into an insert within an amber glass vial.

Before start running samples a new column was set, it is recommended to change the column (Acquity UHPLC C18 1.7μm 2.1x100 mm column) (Waters; Milford, MA, USA) every 1000 samples. After the column change, the new one must be purged for 5 min to take air out. The UHPLC column is coupled to a VanGuard pre-column (Acquity UHPLC C18 1.7μm 2.1x5 mm pre-column Waters), which is replaced after ~330 sample injections. Six “no sample injections” were injected at the beginning of each run to condition the column, followed by ten samples, one pool (made out of the mix of the second aliquot of all the samples contained per UHPLC plate) and one blank.

According to the diversity and amount of lipid species identified under ESI (+) and (-) during an optimization of lipid extraction and lipid analysis (data not shown), we selected ESI positive mode on Agilent® ultra high performance liquid chromatography and quadrupole time of flight mass spectrometry (UHPLC-QTOF MS/MS). The UHPLC-QTOF MS/MS utilized were Agilent 1290 and Agilent 6530, respectively. We injected 1.67 μL per sample into UHPLC-QTOF MS/MS ESI (+), the running time per sample was 15 min. 

## Results

In total we analyzed 150 greenhouse samples from three genotypes at 10 different developmental stages from V4 to V14. We collected tissue from leaves of 6 different ages. For the three youngest blocks the samples were collected along the leaf, but in the block with the most mature plants, the samples were collected at defined locations in the leave (tip, center, base), see Figure 1.
In total around 2500 data points were collected and analyzed. We also analyzed set of 24 plants (8 per genotype) grown in a highland field and sampled at around V3 - V6 developmental stage.

A total of 171 lipid species were identified with lipid profiling. The lipid classes found were: cholesterol esters (CE), diacylglycerols (DG), non-hydroxyacyl sphingosines (CerNS), digalactosyldiacylglycerols (DGDG), glycosphingolipids (GlcCerNS), lysophosphatidylcholines (LPC), monogalactosyldiacylglycerols (MGDG), phosphatidylcholines (PC), phosphoethanolamines (PE), phosphatidylglycerols (PG), plasmenyl phosphatidylcholines (plasmenylPC), sphingomyelins (SM), sulfoquinovosyl diacylglycerols (SQDG) and triaglycerides (TG). See Table 2.

**Table 2. Number of lipid species identified, within each lipid class, using UHPLC-QTOF MS/MS ESI positive mode.** 

| Lipid classes                   | Number of species |
|---------------------------------|-------------------|
| Cholesterol esters              | 1                 |
| Diacylglycerols                 | 7                 |
| Non-hydroxyacyl sphingosine     | 2                 |
| Digalactosyldiacylglycerols     | 21                |
| Glycosphingolipids              | 4                 |
| Lysophosphatidylcholines        | 6                 |
| Monogalactosyldiacylglycerol    | 13                |
| Phosphatidylcholines            | 26                |
| Phosphoethanolamines            | 11                |
| Phosphatidylglycerols           | 8                 |
| Plasmenyl phosphatidylcholines  | 3                 |
| Sphingomyelins                  | 5                 |
| Sulfoquinovosyl diacylglycerols | 7                 |
| Triglycerides                   | 52                |

The average coefficient of variation of the sample pool, analyzed every 10 samples, was in average 9.7% and it ranged between 1.7 and 29.6 %. In the samples analyzed we observed a coefficient of variation  63.9% with a range between 22.7 and 222.8% across all tissues. By genotype, coefficients of variation were 51.9% in the case of B73 with a 14.6-192.8% range, 57.7 with 13.1-223.6% range in CML312 and 58.6% with a 14.0-137.1% range in PT. In the set of plants that were grown in the field coefficients of variation were 25.0% in the case of B73 with a 7.5-60.9% range, 30.6% with a 10.2-76.6% range in CML312 and 28.3 with a 9.1-156.2%.

![Figure3](../output/3Genotypes_sum_lipid_classes_heatmap_lipid_class_average_Pearson_noBorders.png)
**Figure3. Heatmap of the genotype-specific lipid profile detected with UHPLC-QTOF MS/MS, generated by Ward clustering algorithm based on Pearson's correlation.**

![Figure4](../output/3Genotypes_sum_lipid_class_sPLSD_Loadings.png)
**Figure4. Graph of the loadings from the three genotypes used, acrossed all lipid classes**

![Figure5](../output/Genotype_GreenhouseVSField_PCs_LPCSs_sPLSD_Loadings.jpg)
**Figure5. Graph of the loadings of phosphatidylcholine species (PCs) and lysophosphatidylchline species (LPCs) from the plants grown in greenhouse and the ones grown in the highland field, left to right respectively.**

![Figure6](../figures/LPC_18_2_PC_36_6_Greenhouse_vs_Field.jpg)
**Figure6. Comparison of LPC 18:2 and PC 36:6 (18:3, 18:3) between plants grown in greenhouse and highland field samples.**




