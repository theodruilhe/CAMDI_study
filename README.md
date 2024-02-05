# CAMDI Study: Credit Allocation Model DIscrimination Study

## Are credit allocation models discriminating?

### Motivation

Many people on Earth aspire to become homeowners. However, in order to achieve this goal, it is often necessary to obtain a loan. To apply for a loan, you must submit an application. The bank will then evaluate your application and determine if you are eligible. To make this decision, they use a classification model that aims to discriminate between good-type borrowers and bad-type borrowers.

Ethical questions arise when those models include discriminatory variables that are *a priori* not related at all to a loan reimbursement capability, compared to purely financial variables. The objective of this paper is to determine if these variables affect credit allocation. If they do, we aim to measure the difference in rejection rates between the discriminated groups and others.

### Economic Relevance

Empirical studies aiming to identify the determinants of access to credit mostly present results on classic factors. Based on this, Mwangi (2009) make an empirical analysis based on data that contains information on 4420 households in Kenya. Following his study, he established that demographic and socioeconomic characteristics influence one's access to credit. Furthermore, according to him, age, gender or education have an impact on the access to credit. 

In their study, Bayer et al. (2017) give a more specific point of view, focusing on examining racial and ethnic differences in high-cost mortgage lending. In their results, they identified large racial and ethnic differences in the likelihood of receiving a rate spread mortgage in the home purchase market after controlling for detailed borrower and loan attributes.

Concerning gender discrimination, the study of Agier and Sfarz (2011), by focusing on gender influence in small-business lending with a rich database from a Brazilian microfinance institution, reveals a gender bias. Their empirical results show that there is gender discrimination in the sense where women entrepreneurs receive smaller loans, and less often.

Finally, in the case where two applicants apply for a mortgage as a couple, Hagendorf et al. (2022) study the consequences of the legalization of same-sex marriages across the United States. They expose a denial gap between same sex-borrowers and different-sex borrowers, which is not reduced when the company relies less on human loan officers. Thereby, it appears even more pertinent to question the morality of credit allocation models.

### Data and modelling

In this paper, we will use econometric modeling to analyze the issue of discrimination in credit allocation. To empirically test our hypotheses, we will utilize data from the Consumer Financial Protection Bureau (CFPB). Each year, financial institutions report mortgage data to the public under the Home Mortgage Disclosure Act (HMDA). We will focus on analyzing the HMDA data collected from 2007 to 2017.

The variable that we aim to explain with our model is qualitative, specifically the acceptance or rejection of credit. Our first model will be a logistic regression, but we will proceed with caution by justifying thoroughly that the (numerous) assumptions of this model are satisfied and we will particularly account for potential endogeneity issues. Additionally, we will explore non-parametric estimation models to reduce potential bias caused by inappropriate or overly restrictive specifications in our first model. Lastly, we will examine machine learning models such as random forest to compare the accuracy of different methods and to bring us closer to the screening algorithms actually used by banks.
