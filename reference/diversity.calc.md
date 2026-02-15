# Compute Diversity Measures

The function `diversity.calc` calculates the following diversity
measures

- Margalef's Richness Index (\\D\_{Margalef}\\) (Margalef 1973)

- Menhinick's Index (\\D\_{Menhinick}\\) (Menhinick 1964)

- Berger–Parker Index (\\D\_{BP}\\) (Berger and Parker 1970)

- Reciprocal Berger–Parker Index (\\D\_{BP\_{R}}\\) (Magurran 2011)

- Simpson's Index (\\d\\) (Simpson 1949; Peet 1974)

- Simpson's Index of Diversity or Gini's Diversity Index or Gini-Simpson
  Index or Nei's Diversity Index or Nei's Variation Index (\\D\\) or
  Hurlbert’s probability of interspecific encounter (\\PIE\\) (Gini
  1912, 1912; Greenberg 1956; Berger and Parker 1970; Hurlbert 1971; Nei
  1973; Peet 1974)

- Maximum Simpson's Index of Diversity or Maximum Nei's
  Diversity/Variation Index (\\D\_{max}\\) (Hennink and Zeven 1990)

- Simpson's Reciprocal Index or Hill's \\N\_{2}\\ (\\D\_{R}\\) or
  Effective number of Species (\\ENS\_{d}\\) (Williams 1964; Hill 1973)

- Relative Simpson's Index of Diversity or Relative Nei's
  Diversity/Variation Index (\\D'\\) (Hennink and Zeven 1990)

- Simpson’s evenness or equitability (\\D\_{e}\\) (Pielou 1966; Hill
  1973)

- Shannon or Shannon-Weaver or Shannon-Wiener Diversity Index or Shannon
  entropy (\\H\\) (Shannon and Weaver 1949; Peet 1974)

- Maximum Shannon-Weaver Diversity Index (\\H\_{max}\\) (Hennink and
  Zeven 1990)

- Relative Shannon-Weaver Diversity Index or Shannon Equitability Index
  (\\H'\\) or Pielou's Evenness (\\J\\) (Pielou 1966; Hennink and Zeven
  1990)

- Effective number of species for the Shannon - Weaver Diversity Index
  (\\ENS\_{H}\\) or Hill's \\N\_{1}\\ (Macarthur 1965; Hill 1973)

- Heip's Evenness Index (\\E\_{Heip}\\) (Heip 1974)

- McIntosh Diversity Index (\\D\_{Mc}\\) (McIntosh 1967; Peet 1974)

- McIntosh Evenness Index (\\E\_{Mc}\\) (Pielou 1975)

- Smith & Wilson's Evenness Index (\\E\_{var}\\) (Smith and Wilson 1996)

- Brillouin Diversity Index (\\D\_{Brillouin}\\) (Brillouin 2013)

- Rényi Entropy (\\{}^q H\_{Rényi}\\) (Renyi 1960)

- Tsallis or HCDT Entropy (\\{}^q H\_{Tsallis}\\) (Havrda and Charvat
  1967; Daroczy 1970; Tsallis 1988)

- Hill Numbers (\\{}^q D\\) (Hill 1973)

## Usage

``` r
diversity.calc(x, base = exp(1), na.omit = TRUE)
```

## Arguments

- x:

  A factor vector of categories (e.g., species, traits). The frequency
  of each level is treated as the abundance of that category.

- base:

  The logarithm base to be used for computation of shannon family of
  diversity indices. Default is `exp(1)`.

- na.omit:

  logical. If `TRUE`, missing values (`NA`) are ignored and not included
  as a distinct factor level for computation. Default is `TRUE`.

## Value

A list of the different diversity measures computed.

- richness:

  The number of classes in `x` or the richness.

- margalef_index:

  Margalef's Richness Index (\\D\_{Margalef}\\) (Margalef 1973)

- menhinick_index:

  Menhinick's Index (\\D\_{Menhinick}\\) (Menhinick 1964)

- berger_parker:

  Berger–Parker Index (\\D\_{BP}\\) (Berger and Parker 1970)

- berger_parker_reciprocal:

  Reciprocal Berger–Parker Index (\\D\_{BP\_{R}}\\) (Magurran 2011)

- simpson:

  Simpson's index (\\d\\) (Simpson 1949; Peet 1974)

- gini_simpson:

  Simpson's Index of Diversity or Gini's Diversity Index or Gini-Simpson
  Index or Nei's Diversity Index or Nei's Variation Index (\\D\\) or
  Hurlbert’s probability of interspecific encounter (\\PIE\\) (Gini
  1912, 1912; Greenberg 1956; Berger and Parker 1970; Hurlbert 1971; Nei
  1973; Peet 1974)

- simpson_max:

  Maximum Simpson's Index of Diversity or Maximum Nei's
  Diversity/Variation Index (\\D\_{max}\\) (Hennink and Zeven 1990)

- simpson_reciprocal:

  Simpson's Reciprocal Index or Hill's \\N\_{2}\\ (\\D\_{R}\\) or
  Effective number of Species (\\ENS\_{d}\\) (Williams 1964; Hill 1973)

- simpson_relative:

  Relative Simpson's Index of Diversity or Relative Nei's
  Diversity/Variation Index (\\D'\\) (Hennink and Zeven 1990)

- simpson_evenness:

  Simpson’s evenness or equitability (\\D\_{e}\\) (Pielou 1966; Hill
  1973)

- shannon:

  Shannon or Shannon-Weaver or Shannon-Wiener Diversity Index or Shannon
  entropy (\\H\\) (Shannon and Weaver 1949; Peet 1974)

- shannon_max:

  Maximum Shannon-Weaver Diversity Index (\\H\_{max}\\) (Hennink and
  Zeven 1990)

- shannon_relative:

  Relative Shannon-Weaver Diversity Index or Shannon Equitability Index
  (\\H'\\) or Pielou's Evenness (\\J\\) (Pielou 1966; Hennink and Zeven
  1990)

- shannon_ens:

  Effective number of species for the Shannon - Weaver Diversity Index
  (\\ENS\_{H}\\) or Hill's \\N\_{1}\\ (Macarthur 1965; Hill 1973)

- heip_evenness:

  Heip's Evenness Index (\\E\_{Heip}\\) (Heip 1974)

- mcintosh_diversity:

  McIntosh Diversity Index (\\D\_{Mc}\\) (McIntosh 1967; Peet 1974)

- mcintosh_evenness:

  McIntosh Evenness Index (\\E\_{Mc}\\) (Pielou 1975)

- smith_wilson:

  Smith & Wilson's Evenness Index (\\E\_{var}\\) (Smith and Wilson 1996)

- brillouin_index:

  Brillouin Diversity Index (Brillouin 2013)

- renyi_entropy_0:

  Rényi Entropy of order 0 (Renyi 1960)

- renyi_entropy_1:

  Rényi Entropy of order 1

- renyi_entropy_2:

  Rényi Entropy of order 2

- tsallis_entropy_0:

  Tsallis Entropy of order 0 (Havrda and Charvat 1967; Daroczy 1970;
  Tsallis 1988)

- tsallis_entropy_1:

  Tsallis Entropy of order 1

- tsallis_entropy_2:

  Tsallis Entropy of order 2

- hill_number_0:

  Hill Number of order 0 (\\{}^0 D\\) (Hill 1973)

- hill_number_1:

  Hill Number of order 1 (\\{}^1 D\\)

- hill_number_2:

  Hill Number of order 2 (\\{}^2 D\\)

## Details

The diversity indices implemented in `diversity.calc` are computed as
follows.

### Richness Indices

The number of classes of a phenotypic trait (or species richness)
(\\k\\) can be described by adjusting for sample size (\\N\\) in
Margalef's Richness Index (\\D\_{Margalef}\\) (Margalef 1973) and
Menhinick's Index (\\D\_{Menhinick}\\) (Menhinick 1964)

These are computed as follows.

\\D\_{Margalef} = \frac{k - 1}{\ln(N)}\\

\\D\_{Menhinick} = \frac{k}{\sqrt{N}}\\

### Berger–Parker Index

This is the is the proportion of individuals belonging to the most
abundant class in a trait (or species in a community) and is computed as
below.

\\D\_{BP} = \max(p_i)\\

Where, \\p\_{i}\\ denotes the proportion/fraction/frequency of
accessions in the \\i\\th phenotypic class for a trait (or number of
individuals in the \\i\\th species).

It's reciprocal estimates the relative diversity of this class.

\\D\_{BP\_{R}} = \frac{1}{D\_{BP}}\\

### Simpson's and Related Indices

Simpson's index (\\d\\) which estimates the probability that two
accessions randomly selected will belong to the same phenotypic class of
a trait (or species in a community), is computed as follows (Simpson
1949; Peet 1974) .

\\d = \sum\_{i = 1}^{k}p\_{i}^{2}\\

Where, \\k\\ is the number of phenotypic classes for the trait (or
number of species in the community).

The value of \\d\\ can range from 0 to 1 with 0 representing maximum
diversity and 1, no diversity.

\\d\\ is subtracted from 1 to give Simpson's index of diversity (\\D\\)
(Greenberg 1956; Berger and Parker 1970; Hurlbert 1971; Peet 1974;
Hennink and Zeven 1990) originally suggested by Gini (1912, 1912) and
described in literature as Gini's diversity index or Gini-Simpson index.
It is the same as Nei's diversity index or Nei's variation index (Nei
1973; Hennink and Zeven 1990) . Greater the value of \\D\\, greater the
diversity with a range from 0 to 1.

\\D = 1 - d\\

The maximum value of \\D\\, \\D\_{max}\\ occurs when accessions are
uniformly distributed across the phenotypic classes (or individuals are
uniformly distributed across species in a community) and is computed as
follows (Hennink and Zeven 1990) .

\\D\_{max} = 1 - \frac{1}{k}\\

Reciprocal of \\d\\ gives the Simpson's reciprocal index (\\D\_{R}\\)
(Williams 1964; Hennink and Zeven 1990) and can range from 1 to \\k\\.
This was also described in Hill (1973) as \\N\_{2}\\ or as Effective
number of Species (or classes) (\\ENS\_{d}\\).

\\D\_{R} = \frac{1}{d}\\

Relative Simpson's index of diversity or Relative Nei's
diversity/variation index (\\H'\\) (Hennink and Zeven 1990) is defined
as follows (Peet 1974) .

\\D' = \frac{D}{D\_{max}}\\

Simpson’s evenness or equitability (\\D\_{e}\\ is described as follows
(Pielou 1966; Hill 1973) .

\\D\_{e} = \frac{1}{d \cdot k}\\

### Shannon-Weaver and Related Indices

An index of information \\H\\, was described by Shannon and Weaver
(1949) as follows.

\\H = -\sum\_{i=1}^{k}p\_{i} \log\_{2}(p\_{i})\\

\\H\\ is described as Shannon or Shannon-Weaver or Shannon-Wiener
diversity index or Shannon entropy in literature (Shannon and Weaver
1949; Peet 1974) .

Alternatively, \\H\\ is also computed using natural logarithm instead of
logarithm to base 2.

\\H = -\sum\_{i=1}^{k}p\_{i} \ln(p\_{i})\\

The maximum value of \\H\\ (\\H\_{max}\\) is \\\ln(k)\\. This value
occurs when each phenotypic class for a trait has the same proportion of
accessions (or each species in a community has the same proportion of
individuals) (Hennink and Zeven 1990) .

\\H\_{max} = \log\_{2}(k)\\\\ \textrm{OR} \\\\ H\_{max} = \ln(k)\\

The relative Shannon-Weaver diversity index or Shannon equitability
index (\\H'\\) or Pielou's Evenness (\\J\\) is the Shannon diversity
index (\\I\\) divided by the maximum diversity (\\H\_{max}\\) (Pielou
1966; Hennink and Zeven 1990) .

\\H' = \frac{H}{H\_{max}}\\

Macarthur (1965) described the Effective number of species (or classes)
for the Shannon index (\\ENS\_{H}\\) as follows.

\\ENS\_{H} = e^{H}\\

Heip’s index or Heip's Evenness index is a transformation of the
Shannon–Wiener diversity index that standardizes it relative to number
of classes in the trait (or species richness) and is computed as follows
(Heip 1974) .

\\E\_{Heip} = \frac{e^{H} - 1}{k - 1}\\

### McIntosh's Measure of Diversity

A similar index of diversity was described by McIntosh (1967) as follows
(\\D\_{Mc}\\) (Peet 1974) .

\\D\_{Mc} = \frac{N - \sqrt{\sum\_{i=1}^{k}n\_{i}^2}}{N - \sqrt{N}}\\

Where, \\n\_{i}\\ denotes the number of accessions in the \\i\\th
phenotypic class for a trait (or number of individuals in the \\i\\th
species in the community) and \\N\\ is the total number of accessions so
that \\p\_{i} = {n\_{i}}/{N}\\.

An additional measure of evenness was proposed by Pielou (1975) as
follows.

\\E\_{Mc} = \frac{N - \sqrt{\sum\_{i=1}^{k}n\_{i}^2}}{N -
\frac{N}{\sqrt{S}}}\\

### Smith & Wilson's Evenness Index

This index measures how equally are accessions/genotypes distributed
among different trait classes (or individuals individuals are
distributed among different species in a community). This is less
sensitive to rare classes or species and is computed as follows.

\\E\_{var} = 1 - \frac{2}{\pi} \arctan{\left ( \frac{1}{k}
\sum\_{i=1}^{k}(\ln{n\_{i} - \overline{\ln{n}}})^{2} \right )}\\

### Brillouin Diversity Index

This is an information-theoretic measure appropriate for complete
censuses and is computed as follows (Brillouin 2013) .

\\H\_{B} = \frac{\ln(N!) - \sum \ln(n_i!)}{N}\\

### Parametric Indices

Parametric indices, also known as multivariate or compound indices, use
a sensitivity parameter (\\q\\) to weigh frequent and rare classes
within a trait (or common or rare species within a community).

The Rényi entropy extends several entropy measures, including Shannon
entropy, and is computed as follows (Renyi 1960) .

\\{}^q H\_{Rényi} = \frac{1}{1-q} \ln \sum\_{i=1}^{k} p\_{i}^{q} , \quad
q \ge 0, q \neq 1\\

It is more frequently computed using natural logarithm instead of
logarithm to base 2. The index is undefined for (\\q = 1\\), but Shannon
entropy is as a limiting case.

Tsallis proposed a similar measure, the HCDT or Tsallis entropy (Havrda
and Charvat 1967; Daroczy 1970; Tsallis 1988) , which matches species
richness for \\q = 0\\, Shannon entropy \\q = 1\\, and the Gini-Simpson
index for \\q = 2\\.

\\{}^q H\_{Tsallis} = \frac{1}{q - 1} \left ( 1 - \sum\_{i=1}^{k} p_i^q
\right ), \quad q \ge 0, q \neq 1\\

Hill showed that species richness, Shannon entropy, and Simpson's index
are all related diversity indices, collectively known as Hill numbers
which is defined as below (Hill 1973) .

\\{}^q D = {\left ( \sum\_{i=1}^{k} p\_{i}^{q} \right )}^{\frac{1}{1-q}}
, \quad q \ge 0, q \neq 1\\

Where,

\\{}^0 D = k\\

\\{}^1 D = e^{H}\\

\\{}^2 D = D\_{R}\\

## References

Berger WH, Parker FL (1970). “Diversity of planktonic foraminifera in
deep-sea sediments.” *Science*, **168**(3937), 1345–1347.  
  
Brillouin L (2013). *Science and information theory*, Dover edition.
Dover Publications, Inc., Mineola, New York. ISBN 978-0-486-31641-3.  
  
Daroczy Z (1970). “Generalized information functions.” *Information and
Control*, **16**(1), 36–51.  
  
Gini C (1912). *Variabilita e Mutabilita. Contributo allo Studio delle
Distribuzioni e delle Relazioni Statistiche. \[Fasc. I.\]*. Tipogr. di
P. Cuppini, Bologna.  
  
Gini C (1912). “Variabilita e mutabilita.” In Pizetti E, Salvemini T
(eds.), *Memorie di Metodologica Statistica*. Liberia Eredi Virgilio
Veschi, Roma, Italy.  
  
Greenberg JH (1956). “The measurement of linguistic diversity.”
*Language*, **32**(1), 109.  
  
Havrda J, Charvat F (1967). “Quantification method of classification
processes. Concept of structural \<p\>&alpha;\</p\>-entropy.”
*Kybernetika*, **3**(1), (30)–35.  
  
Heip C (1974). “A new index measuring evenness.” *Journal of the Marine
Biological Association of the United Kingdom*, **54**(3), 555–557.  
  
Hennink S, Zeven AC (1990). “The interpretation of Nei and
Shannon-Weaver within population variation indices.” *Euphytica*,
**51**(3), 235–240.  
  
Hill MO (1973). “Diversity and evenness: A unifying notation and its
consequences.” *Ecology*, **54**(2), 427–432.  
  
Hurlbert SH (1971). “The nonconcept of species diversity: a critique and
alternative parameters.” *Ecology*, **52**(4), 577–586.  
  
Macarthur RH (1965). “Patterns of species diversity.” *Biological
Reviews*, **40**(4), 510–533.  
  
Magurran AE (2011). *Measuring biological diversity*, 9 \[Nachdr.\]
edition. Blackwell, Malden, Mass. ISBN 978-0-632-05633-0.  
  
Margalef R (1973). “Information theory in ecology.” *International
Journal of General Systems*, **3**, 36–71.  
  
McIntosh RP (1967). “An index of diversity and the relation of certain
concepts to diversity.” *Ecology*, **48**(3), 392–404.  
  
Menhinick EF (1964). “A comparison of some species-individuals diversity
indices applied to samples of field insects.” *Ecology*, **45**(4),
859–861.  
  
Nei M (1973). “Analysis of gene diversity in subdivided populations.”
*Proceedings of the National Academy of Sciences*, **70**(12),
3321–3323.  
  
Peet RK (1974). “The measurement of species diversity.” *Annual Review
of Ecology and Systematics*, **5**(1), 285–307.  
  
Pielou EC (1966). “The measurement of diversity in different types of
biological collections.” *Journal of Theoretical Biology*, **13**,
131–144.  
  
Pielou EC (1975). *Ecological diversity*. New York : Wiley. ISBN
978-0-471-68925-6.  
  
Renyi A (1960). “On measures of entropy and information.” In Neyman J
(ed.), *Proceedings of the Fourth Berkeley Symposium on Mathematical
Statistics and Probability (June 20-July 30 1960), Volume I:
Contributions to the Theory of Statistics*, 547–561. University of
California Press.  
  
Shannon CE, Weaver W (1949). *The Mathematical Theory of Communication*,
number v. 2 in The Mathematical Theory of Communication. University of
Illinois Press.  
  
Simpson EH (1949). “Measurement of diversity.” *Nature*, **163**(4148),
688–688.  
  
Smith B, Wilson JB (1996). “A consumer's guide to evenness indices.”
*Oikos*, **76**(1), 70.  
  
Tsallis C (1988). “Possible generalization of Boltzmann-Gibbs
statistics.” *Journal of Statistical Physics*, **52**(1-2), 479–487.  
  
Williams CB (1964). *Patterns in the Balance of Nature and Related
Problems in Quantitative Ecology*. Academic Press.

## Examples

``` r
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Qualitative trait data ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(EvaluateCore)

pdata <- cassava_CC

qual <- c("CUAL", "LNGS", "PTLC", "DSTA", "LFRT", "LBTEF", "CBTR", "NMLB",
          "ANGB", "CUAL9M", "LVC9M", "TNPR9M", "PL9M", "STRP", "STRC",
          "PSTR")

# Convert qualitative data columns to factor
pdata[, qual] <- lapply(pdata[, qual], as.factor)

# Get diversity measures
diversity.calc(x = pdata$CUAL)
#> $richness
#> [1] 4
#> 
#> $margalef_index
#> [1] 0.5854842
#> 
#> $menhinick_index
#> [1] 0.3086067
#> 
#> $berger_parker
#> [1] 0.5297619
#> 
#> $berger_parker_reciprocal
#> [1] 1.88764
#> 
#> $simpson
#> [1] 0.3784722
#> 
#> $gini_simpson
#> [1] 0.6215278
#> 
#> $simpson_max
#> [1] 0.75
#> 
#> $simpson_reciprocal
#> [1] 2.642202
#> 
#> $simpson_relative
#> [1] 0.8287037
#> 
#> $simpson_evenness
#> [1] 0.4022346
#> 
#> $shannon
#> [1] 1.113994
#> 
#> $shannon_max
#> [1] 1.386294
#> 
#> $shannon_relative
#> [1] 0.803577
#> 
#> $shannon_ens
#> [1] 3.046503
#> 
#> $heip_evenness
#> [1] 1.329531
#> 
#> $mcintosh_diversity
#> [1] 0.4169689
#> 
#> $mcintosh_evenness
#> [1] 0.7695981
#> 
#> $smith_wilson
#> [1] 0.4591224
#> 
#> $brillouin_index
#> [1] 1.072686
#> 
#> $renyi_entropy_0
#> [1] 1.386294
#> 
#> $renyi_entropy_1
#> [1] 1.113994
#> 
#> $renyi_entropy_2
#> [1] 0.9716126
#> 
#> $tsallis_entropy_0
#> [1] 3
#> 
#> $tsallis_entropy_1
#> [1] 1.113994
#> 
#> $tsallis_entropy_2
#> [1] 0.6215278
#> 
#> $hill_number_0
#> [1] 4
#> 
#> $hill_number_1
#> [1] 3.046503
#> 
#> $hill_number_2
#> [1] 2.642202
#> 

# Get diversity measures for multiple qualitative traits
div_out1 <- lapply(pdata[, qual], diversity.calc)
do.call(rbind, div_out1)
#>        richness margalef_index menhinick_index berger_parker
#> CUAL   4        0.5854842      0.3086067       0.5297619    
#> LNGS   3        0.3903228      0.231455        0.4285714    
#> PTLC   5        0.7806456      0.3857584       0.5892857    
#> DSTA   5        0.7806456      0.3857584       0.4404762    
#> LFRT   4        0.5854842      0.3086067       0.5238095    
#> LBTEF  6        0.975807       0.46291         0.2559524    
#> CBTR   3        0.3903228      0.231455        0.5595238    
#> NMLB   9        1.561291       0.6943651       0.3214286    
#> ANGB   4        0.5854842      0.3086067       0.452381     
#> CUAL9M 5        0.7806456      0.3857584       0.3333333    
#> LVC9M  5        0.7806456      0.3857584       0.5          
#> TNPR9M 5        0.7806456      0.3857584       0.3214286    
#> PL9M   2        0.1951614      0.1543033       0.5119048    
#> STRP   4        0.5854842      0.3086067       0.3392857    
#> STRC   2        0.1951614      0.1543033       0.5952381    
#> PSTR   2        0.1951614      0.1543033       0.6666667    
#>        berger_parker_reciprocal simpson   gini_simpson simpson_max
#> CUAL   1.88764                  0.3784722 0.6215278    0.75       
#> LNGS   2.333333                 0.3877551 0.6122449    0.6666667  
#> PTLC   1.69697                  0.4213435 0.5786565    0.8        
#> DSTA   2.27027                  0.3035006 0.6964994    0.8        
#> LFRT   1.909091                 0.4265873 0.5734127    0.75       
#> LBTEF  3.906977                 0.2005385 0.7994615    0.8333333  
#> CBTR   1.787234                 0.4872449 0.5127551    0.6666667  
#> NMLB   3.111111                 0.1980584 0.8019416    0.8888889  
#> ANGB   2.210526                 0.3421202 0.6578798    0.75       
#> CUAL9M 3                        0.2880527 0.7119473    0.8        
#> LVC9M  2                        0.3886763 0.6113237    0.8        
#> TNPR9M 3.111111                 0.2218679 0.7781321    0.8        
#> PL9M   1.953488                 0.5002834 0.4997166    0.5        
#> STRP   2.947368                 0.3118622 0.6881378    0.75       
#> STRC   1.68                     0.5181406 0.4818594    0.5        
#> PSTR   1.5                      0.5555556 0.4444444    0.5        
#>        simpson_reciprocal simpson_relative simpson_evenness shannon  
#> CUAL   2.642202           0.8287037        0.4022346        1.113994 
#> LNGS   2.578947           0.9183673        0.5444444        1.004242 
#> PTLC   2.37336            0.7233206        0.3456282        1.113785 
#> DSTA   3.294887           0.8706243        0.2871503        1.349228 
#> LFRT   2.344186           0.7645503        0.4359862        0.9661827
#> LBTEF  4.986572           0.9593537        0.2084737        1.651889 
#> CBTR   2.052356           0.7691327        0.6500829        0.778669 
#> NMLB   5.049016           0.9021843        0.1385526        1.775015 
#> ANGB   2.922949           0.8771731        0.3800086        1.176272 
#> CUAL9M 3.471587           0.8899341        0.2809197        1.335612 
#> LVC9M  2.572835           0.7641546        0.3271589        1.10697  
#> TNPR9M 4.507186           0.9726651        0.2570258        1.557711 
#> PL9M   1.998867           0.9994331        1.000567         0.6928637
#> STRP   3.206544           0.917517         0.3632994        1.21246  
#> STRC   1.929978           0.9637188        1.037647         0.6748953
#> PSTR   1.8                0.8888889        1.125            0.6365142
#>        shannon_max shannon_relative shannon_ens heip_evenness
#> CUAL   1.386294    0.803577         3.046503    1.329531     
#> LNGS   1.098612    0.9141009        2.729839    1.629034     
#> PTLC   1.609438    0.6920334        3.045865    0.9967717    
#> DSTA   1.609438    0.8383227        3.85445     1.501076     
#> LFRT   1.386294    0.6969535        2.627894    1.010189     
#> LBTEF  1.791759    0.9219369        5.216826    1.967847     
#> CBTR   1.098612    0.7087751        2.178571    1.037618     
#> NMLB   2.197225    0.807844         5.900367    1.493279     
#> ANGB   1.386294    0.8485006        3.242263    1.485852     
#> CUAL9M 1.609438    0.8298622        3.802321    1.467013     
#> LVC9M  1.609438    0.6877994        3.025179    0.9845744    
#> TNPR9M 1.609438    0.9678601        4.74794     2.115542     
#> PL9M   0.6931472   0.999591         1.999433    1.71717      
#> STRP   1.386294    0.8746048        3.361743    1.583352     
#> STRC   0.6931472   0.9736681        1.963827    1.647638     
#> PSTR   0.6931472   0.9182958        1.889882    1.505018     
#>        mcintosh_diversity mcintosh_evenness smith_wilson brillouin_index
#> CUAL   0.4169689          0.7695981         0.4591224    1.072686       
#> LNGS   0.4088431          0.8927017         0.6401521    0.9736063      
#> PTLC   0.3802252          0.6347663         0.4467806    1.06311        
#> DSTA   0.4866359          0.8124135         0.5123229    1.294889       
#> LFRT   0.3758619          0.693727          0.3336841    0.9291107      
#> LBTEF  0.5983483          0.9331358         0.5031023    1.584527       
#> CBTR   0.327216           0.7144704         0.3314674    0.7525541      
#> NMLB   0.6013583          0.8324437         0.3647054    1.686068       
#> ANGB   0.4497918          0.8301792         0.5028364    1.133948       
#> CUAL9M 0.5020268          0.8381077         0.4522341    1.282613       
#> LVC9M  0.408042           0.6812051         0.3707252    1.058812       
#> TNPR9M 0.5731943          0.9569183         0.7843173    1.49946        
#> PL9M   0.3171624          0.9993158         0.9785679    0.6762626      
#> STRP   0.4784684          0.8831074         0.4699856    1.170253       
#> STRC   0.3036037          0.9565949         0.8305121    0.6584021      
#> PSTR   0.2759327          0.869409          0.7098798    0.6202605      
#>        renyi_entropy_0 renyi_entropy_1 renyi_entropy_2 tsallis_entropy_0
#> CUAL   1.386294        1.113994        0.9716126       3                
#> LNGS   1.098612        1.004242        0.9473813       2                
#> PTLC   1.609438        1.113785        0.8643068       4                
#> DSTA   1.609438        1.349228        1.192372        4                
#> LFRT   1.386294        0.9661827       0.8519382       3                
#> LBTEF  1.791759        1.651889        1.606749        5                
#> CBTR   1.098612        0.778669        0.7189884       2                
#> NMLB   2.197225        1.775015        1.619193        8                
#> ANGB   1.386294        1.176272        1.072593        3                
#> CUAL9M 1.609438        1.335612        1.244612        4                
#> LVC9M  1.609438        1.10697         0.9450084       4                
#> TNPR9M 1.609438        1.557711        1.505673        4                
#> PL9M   0.6931472       0.6928637       0.6925804       1                
#> STRP   1.386294        1.21246         1.165194        3                
#> STRC   0.6931472       0.6748953       0.6575087       1                
#> PSTR   0.6931472       0.6365142       0.5877867       1                
#>        tsallis_entropy_1 tsallis_entropy_2 hill_number_0 hill_number_1
#> CUAL   1.113994          0.6215278         4             3.046503     
#> LNGS   1.004242          0.6122449         3             2.729839     
#> PTLC   1.113785          0.5786565         5             3.045865     
#> DSTA   1.349228          0.6964994         5             3.85445      
#> LFRT   0.9661827         0.5734127         4             2.627894     
#> LBTEF  1.651889          0.7994615         6             5.216826     
#> CBTR   0.778669          0.5127551         3             2.178571     
#> NMLB   1.775015          0.8019416         9             5.900367     
#> ANGB   1.176272          0.6578798         4             3.242263     
#> CUAL9M 1.335612          0.7119473         5             3.802321     
#> LVC9M  1.10697           0.6113237         5             3.025179     
#> TNPR9M 1.557711          0.7781321         5             4.74794      
#> PL9M   0.6928637         0.4997166         2             1.999433     
#> STRP   1.21246           0.6881378         4             3.361743     
#> STRC   0.6748953         0.4818594         2             1.963827     
#> PSTR   0.6365142         0.4444444         2             1.889882     
#>        hill_number_2
#> CUAL   2.642202     
#> LNGS   2.578947     
#> PTLC   2.37336      
#> DSTA   3.294887     
#> LFRT   2.344186     
#> LBTEF  4.986572     
#> CBTR   2.052356     
#> NMLB   5.049016     
#> ANGB   2.922949     
#> CUAL9M 3.471587     
#> LVC9M  2.572835     
#> TNPR9M 4.507186     
#> PL9M   1.998867     
#> STRP   3.206544     
#> STRC   1.929978     
#> PSTR   1.8          

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Species abundance data ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(vegan)
#> Loading required package: permute
data(dune)

abundance_site1 <- unlist(dune[1, ])
abundance_site1[abundance_site1 != 0]
#> Achimill Elymrepe Lolipere  Poaprat  Poatriv 
#>        1        4        7        4        2 

# Convert to raw counts for use in diversity.calc using rep
abundance_site1_raw <- factor(rep(names(abundance_site1),
                                  times = abundance_site1))

# Get diversity measures
diversity.calc(x = abundance_site1_raw)
#> $richness
#> [1] 5
#> 
#> $margalef_index
#> [1] 1.383905
#> 
#> $menhinick_index
#> [1] 1.178511
#> 
#> $berger_parker
#> [1] 0.3888889
#> 
#> $berger_parker_reciprocal
#> [1] 2.571429
#> 
#> $simpson
#> [1] 0.2654321
#> 
#> $gini_simpson
#> [1] 0.7345679
#> 
#> $simpson_max
#> [1] 0.8
#> 
#> $simpson_reciprocal
#> [1] 3.767442
#> 
#> $simpson_relative
#> [1] 0.9182099
#> 
#> $simpson_evenness
#> [1] 0.2722689
#> 
#> $shannon
#> [1] 1.440482
#> 
#> $shannon_max
#> [1] 1.609438
#> 
#> $shannon_relative
#> [1] 0.8950216
#> 
#> $shannon_ens
#> [1] 4.22273
#> 
#> $heip_evenness
#> [1] 1.74747
#> 
#> $mcintosh_diversity
#> [1] 0.6343064
#> 
#> $mcintosh_evenness
#> [1] 0.8770096
#> 
#> $smith_wilson
#> [1] 0.5900996
#> 
#> $brillouin_index
#> [1] 1.156724
#> 
#> $renyi_entropy_0
#> [1] 1.609438
#> 
#> $renyi_entropy_1
#> [1] 1.440482
#> 
#> $renyi_entropy_2
#> [1] 1.326396
#> 
#> $tsallis_entropy_0
#> [1] 4
#> 
#> $tsallis_entropy_1
#> [1] 1.440482
#> 
#> $tsallis_entropy_2
#> [1] 0.7345679
#> 
#> $hill_number_0
#> [1] 5
#> 
#> $hill_number_1
#> [1] 4.22273
#> 
#> $hill_number_2
#> [1] 3.767442
#> 

# Convert multiple site abundance data to raw counts
abundance_site_raw <- apply(dune, 1, function(x) {
  factor(rep(names(x), times = x))
})

# Get diversity measures for multiple sites
div_out2 <- lapply(abundance_site_raw, diversity.calc)
do.call(rbind, div_out2)
#>    richness margalef_index menhinick_index berger_parker
#> 1  5        1.383905       1.178511        0.3888889    
#> 2  10       2.407917       1.543033        0.1666667    
#> 3  10       2.439765       1.581139        0.175        
#> 4  13       3.152368       1.937926        0.1777778    
#> 5  14       3.456344       2.13498         0.1395349    
#> 6  11       2.583178       1.587713        0.125        
#> 7  13       3.25302        2.05548         0.15         
#> 8  12       2.981935       1.897367        0.125        
#> 9  13       3.210557       2.005944        0.1428571    
#> 10 12       2.924598       1.829983        0.1395349    
#> 11 9        2.308312       1.59099         0.21875      
#> 12 9        2.250131       1.521278        0.2285714    
#> 13 10       2.573997       1.740777        0.2727273    
#> 14 7        1.887948       1.428869        0.25         
#> 15 8        2.232503       1.668115        0.2173913    
#> 16 8        2.001998       1.392621        0.2424242    
#> 17 7        2.215616       1.807392        0.2666667    
#> 18 9        2.427305       1.732051        0.2222222    
#> 19 9        2.329653       1.616448        0.1935484    
#> 20 8        2.038447       1.436842        0.1612903    
#>    berger_parker_reciprocal simpson    gini_simpson simpson_max
#> 1  2.571429                 0.2654321  0.7345679    0.8        
#> 2  6                        0.1099773  0.8900227    0.9        
#> 3  5.714286                 0.12125    0.87875      0.9        
#> 4  5.625                    0.09925926 0.9007407    0.9230769  
#> 5  7.166667                 0.08599243 0.9140076    0.9285714  
#> 6  8                        0.09982639 0.9001736    0.9090909  
#> 7  6.666667                 0.0925     0.9075       0.9230769  
#> 8  8                        0.09125    0.90875      0.9166667  
#> 9  7                        0.08843537 0.9115646    0.9230769  
#> 10 7.166667                 0.09680909 0.9031909    0.9166667  
#> 11 4.571429                 0.1328125  0.8671875    0.8888889  
#> 12 4.375                    0.1314286  0.8685714    0.8888889  
#> 13 3.666667                 0.1478421  0.8521579    0.9        
#> 14 4                        0.1666667  0.8333333    0.8571429  
#> 15 4.6                      0.1493384  0.8506616    0.875      
#> 16 4.125                    0.1570248  0.8429752    0.875      
#> 17 3.75                     0.1644444  0.8355556    0.8571429  
#> 18 4.5                      0.138546   0.861454     0.8888889  
#> 19 5.166667                 0.1259105  0.8740895    0.8888889  
#> 20 6.2                      0.132154   0.867846     0.875      
#>    simpson_reciprocal simpson_relative simpson_evenness shannon  shannon_max
#> 1  3.767442           0.9182099        0.2722689        1.440482 1.609438   
#> 2  9.092784           0.9889141        0.1123567        2.252516 2.302585   
#> 3  8.247423           0.9763889        0.113798         2.193749 2.302585   
#> 4  10.07463           0.9758025        0.0853998        2.426779 2.564949   
#> 5  11.62893           0.9843158        0.07814877       2.544421 2.639057   
#> 6  10.01739           0.990191         0.1009906        2.345946 2.397895   
#> 7  10.81081           0.983125         0.08476372       2.471733 2.564949   
#> 8  10.9589            0.9913636        0.09170105       2.434898 2.484907   
#> 9  11.30769           0.9875283        0.08438576       2.493568 2.564949   
#> 10 10.32961           0.9852992        0.09226547       2.398613 2.484907   
#> 11 7.529412           0.9755859        0.1281281        2.106065 2.197225   
#> 12 7.608696           0.9771429        0.127924         2.114495 2.197225   
#> 13 6.763975           0.9468422        0.1173491        2.099638 2.302585   
#> 14 6                  0.9722222        0.1714286        1.86368  1.94591    
#> 15 6.696203           0.9721847        0.1469444        1.979309 2.079442   
#> 16 6.368421           0.9634002        0.1482843        1.959795 2.079442   
#> 17 6.081081           0.9748148        0.1709726        1.876274 1.94591    
#> 18 7.217822           0.9691358        0.1289809        2.079387 2.197225   
#> 19 7.942149           0.9833507        0.1271164        2.134024 2.197225   
#> 20 7.566929           0.991824         0.1440348        2.04827  2.079442   
#>    shannon_relative shannon_ens heip_evenness mcintosh_diversity
#> 1  0.8950216        4.22273     1.74747       0.6343064         
#> 2  0.9782554        9.51164     2.753606      0.7903209         
#> 3  0.9527332        8.968777    2.520738      0.7742024         
#> 4  0.9461313        11.32235    2.67933       0.8049388         
#> 5  0.9641403        12.73586    2.944944      0.8339282         
#> 6  0.9783356        10.44315    2.85028       0.7994354         
#> 7  0.9636577        11.84296    2.864442      0.8265511         
#> 8  0.9798749        11.41465    2.958414      0.8290003         
#> 9  0.9721705        12.10439    2.958778      0.830817          
#> 10 0.9652727        11.00789    2.802893      0.8128109         
#> 11 0.9585114        8.215847    2.484002      0.7720451         
#> 12 0.9623483        8.285403    2.515928      0.7671394         
#> 13 0.911861         8.163211    2.186597      0.7452246         
#> 14 0.9577421        6.44742     2.285477      0.7435226         
#> 15 0.9518463        7.237739    2.340544      0.7751964         
#> 16 0.9424621        7.097871    2.271604      0.7309845         
#> 17 0.9642139        6.529129    2.330436      0.8014042         
#> 18 0.9463699        7.999566    2.385495      0.7773914         
#> 19 0.9712362        8.448796    2.591391      0.7864035         
#> 20 0.9850099        7.754478    2.600327      0.7758096         
#>    mcintosh_evenness smith_wilson brillouin_index renyi_entropy_0
#> 1  0.8770096         0.5900996    1.156724        1.609438       
#> 2  0.9774771         0.7851387    1.930319        2.302585       
#> 3  0.9532272         0.6925466    1.86802         2.302585       
#> 4  0.947825          0.6687728    2.056517        2.564949       
#> 5  0.9645393         0.7407779    2.132228        2.639057       
#> 6  0.9793242         0.7749676    2.02765         2.397895       
#> 7  0.9629308         0.7433868    2.06382         2.564949       
#> 8  0.9811605         0.7830292    2.047512        2.484907       
#> 9  0.9722815         0.7640758    2.095951        2.564949       
#> 10 0.968416          0.7006824    2.035221        2.484907       
#> 11 0.9533483         0.733735     1.75707         2.197225       
#> 12 0.9562038         0.7463774    1.784664        2.197225       
#> 13 0.9001501         0.6387338    1.740005        2.302585       
#> 14 0.951315          0.7317419    1.524661        1.94591        
#> 15 0.9491221         0.6916521    1.590951        2.079442       
#> 16 0.9339309         0.6971933    1.654509        2.079442       
#> 17 0.9557051         0.7576586    1.417032        1.94591        
#> 18 0.9416736         0.686416     1.693929        2.197225       
#> 19 0.9677419         0.7738024    1.774003        2.197225       
#> 20 0.9845671         0.814859     1.720004        2.079442       
#>    renyi_entropy_1 renyi_entropy_2 tsallis_entropy_0 tsallis_entropy_1
#> 1  1.440482        1.326396        4                 1.440482         
#> 2  2.252516        2.207481        9                 2.252516         
#> 3  2.193749        2.109901        9                 2.193749         
#> 4  2.426779        2.31002         12                2.426779         
#> 5  2.544421        2.453496        13                2.544421         
#> 6  2.345946        2.304323        10                2.345946         
#> 7  2.471733        2.380547        12                2.471733         
#> 8  2.434898        2.394152        11                2.434898         
#> 9  2.493568        2.425483        12                2.493568         
#> 10 2.398613        2.335014        11                2.398613         
#> 11 2.106065        2.018817        8                 2.106065         
#> 12 2.114495        2.029292        8                 2.114495         
#> 13 2.099638        1.911611        9                 2.099638         
#> 14 1.86368         1.791759        6                 1.86368          
#> 15 1.979309        1.901541        7                 1.979309         
#> 16 1.959795        1.851352        7                 1.959795         
#> 17 1.876274        1.805182        6                 1.876274         
#> 18 2.079387        1.976553        8                 2.079387         
#> 19 2.134024        2.072184        8                 2.134024         
#> 20 2.04827         2.023787        7                 2.04827          
#>    tsallis_entropy_2 hill_number_0 hill_number_1 hill_number_2
#> 1  0.7345679         5             4.22273       3.767442     
#> 2  0.8900227         10            9.51164       9.092784     
#> 3  0.87875           10            8.968777      8.247423     
#> 4  0.9007407         13            11.32235      10.07463     
#> 5  0.9140076         14            12.73586      11.62893     
#> 6  0.9001736         11            10.44315      10.01739     
#> 7  0.9075            13            11.84296      10.81081     
#> 8  0.90875           12            11.41465      10.9589      
#> 9  0.9115646         13            12.10439      11.30769     
#> 10 0.9031909         12            11.00789      10.32961     
#> 11 0.8671875         9             8.215847      7.529412     
#> 12 0.8685714         9             8.285403      7.608696     
#> 13 0.8521579         10            8.163211      6.763975     
#> 14 0.8333333         7             6.44742       6            
#> 15 0.8506616         8             7.237739      6.696203     
#> 16 0.8429752         8             7.097871      6.368421     
#> 17 0.8355556         7             6.529129      6.081081     
#> 18 0.861454          9             7.999566      7.217822     
#> 19 0.8740895         9             8.448796      7.942149     
#> 20 0.867846          8             7.754478      7.566929     
```
