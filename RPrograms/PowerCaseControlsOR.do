

* Calculating minimum detectable OR for Arias VENOUS study
* Assumptions:
Total sample size to enroll: 1000
Two assumptions: 50/50 VRE/VSE or 70/30 VRE/VSE

VSE Rate of Mortality: 20%
VSE Rate of Microbiological Failure: 19%
VSE RAte of Recurrence: 3%

*Calculate detectable OR for 50/50
power twoproportions .2, test(chi2)  power(0.8) n(1000) 
power twoproportions .2, test(chi2) effect(oratio) power(0.8) n(1000) 

power twoproportions .19, test(chi2)  power(0.8) n(1000) 
power twoproportions .19, test(chi2) effect(oratio) power(0.8) n(1000) 

power twoproportions .03, test(chi2)  power(0.8) n(1000) 
power twoproportions .03, test(chi2) effect(oratio) power(0.8) n(1000) 

*Calculate detectable OR for 70/30
power twoproportions .2, test(chi2)  power(0.8) n(1000) nratio(2.3333333)
power twoproportions .2, test(chi2) effect(oratio) power(0.8) n(1000) nratio(2.3333333)

power twoproportions .19, test(chi2)  power(0.8) n(1000) nratio(2.3333333)
power twoproportions .19, test(chi2) effect(oratio) power(0.8) n(1000) nratio(2.3333333)

power twoproportions .03, test(chi2)  power(0.8) n(1000) nratio(2.3333333)
power twoproportions .03, test(chi2) effect(oratio) power(0.8) n(1000) nratio(2.3333333)

* ALTERNATIVE HIGHER RATES IN VSE GROUP
*Calculate detectable OR for 50/50
power twoproportions .3, test(chi2)  power(0.8) n(1000) 
power twoproportions .3, test(chi2) effect(oratio) power(0.8) n(1000) 

power twoproportions .25, test(chi2)  power(0.8) n(1000) 
power twoproportions .25, test(chi2) effect(oratio) power(0.8) n(1000) 

power twoproportions .06, test(chi2)  power(0.8) n(1000) 
power twoproportions .06, test(chi2) effect(oratio) power(0.8) n(1000) 

*Calculate detectable OR for 70/30
power twoproportions .3, test(chi2)  power(0.8) n(1000) nratio(2.3333333)
power twoproportions .3, test(chi2) effect(oratio) power(0.8) n(1000) nratio(2.3333333)

power twoproportions .25, test(chi2)  power(0.8) n(1000) nratio(2.3333333)
power twoproportions .25, test(chi2) effect(oratio) power(0.8) n(1000) nratio(2.3333333)

power twoproportions .06, test(chi2)  power(0.8) n(1000) nratio(2.3333333)
power twoproportions .06, test(chi2) effect(oratio) power(0.8) n(1000) nratio(2.3333333)

power twoproportions .2, test(chi2) oratio(8) power(0.9)  nratio(1.5)

power twoproportions .2 .7, test(chi2)  nratio(.667)
