# Willingness-to-Volunteer and Stability of Preferences between Cities: Estimating the Benefits of Stormwater Management


This repo contains data and code for reproducing Ando *et al*. (2019), "[Willingness-to-Volunteer and Stability of Preferences between Cities: Estimating the Benefits of Stormwater Management](http://dx.doi.org/)". 

> **Abstract:** Urbanization strains existing stormwater systems, yielding high flood rates, degraded urban aquatic habitat, and low water quality in lakes and rivers. Cities increasingly rely on green infrastructure stormwater solutions that can be maintained in part by volunteers. This paper uses a choice experiment survey in two major U.S. cities – Chicago, Illinois and Portland, Oregon – to estimate the benefits of stormwater management improvement in terms of stated willingness to pay (WTP) money and willingness to volunteer (WTV) time. We find that stormwater management can produce large bundles of benefits. Estimates of WTP are largely (though not comprehensively) stable across cities, but WTV for several benefits is higher in Portland. Finally, while people are willing to volunteer time for some amenities consistent with time valued at 1/3 the average wage rate, a person’s WTV time is not correlated with their own wage rate and people appear to gain positive utility from volunteering.

Click on the "fork" button at the very top right of the page to create an independent copy of the repo within your own GitHub account. Alternatively, click on the green "clone or download" button just below that to download the repo to your local computer. You will need to set the working directory of the `.do` and `.R` files to the location of your download (this is noted at the top of each script). 

The main file for conducting the analysis is `Table_2.do`. This will provide the primary findings in the paper. Each file is stand-alone and should be self-explanatory.

## Requirements

The analysis uses a combination of *Stata* and *R*. The bulk of the work is completed in *Stata*, and the grid search maximization is completed in *R* (Swait and Louviere, 1993).

While *Stata* is proprietary software, *R* is free, open-source and available for download [here](https://www.r-project.org/). If you do not have access to *Stata*, all analyses can be replicated in *R* using the `mlogit` package [here](https://www.rdocumentation.org/packages/mlogit/versions/0.1-0/topics/mlogit).

## Performance

The analyses in this paper involves many computationally intensive random parameter logit estimations. Of the 37 regressions included in this paper, the computation time will range from 1.5 hours to 6.5 days using *Stata* and 4 cores with 64GB RAM.

## Problems

If any errors are discovered, please inform the owner of this repository at bparthum@gmail.com. Thank you, and enjoy!

## License

The software code contained within this repository is made available under the [MIT license](http://opensource.org/licenses/mit-license.php). The data and figures are made available under the [Creative Commons Attribution 4.0](https://creativecommons.org/licenses/by/4.0/) license.
