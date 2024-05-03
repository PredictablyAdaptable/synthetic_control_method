#Synethetic Control Method

1. the synthetic control method is the one of the newest development in quasi-experimentation, where the "treatment effect" is compared against a control group that is snyetically created known as the "synthetic control". 
2. the synthetic control is currently the closest way to create an unbiased control group outside traditional Randomized Trials, and is created by sampleing from the data population.
3. the method of sampling from the existing data population set is done through first looking at the observed variable charactoristics of treatmenet group pre-evaluation, and sampling from the reserve pool ('non treament observations') that has the closest correlation to the treatment group.
4. the weights are typically determined through ml methods such as feature elimination, after quantifying and defining the feature weights and distance algorithmn.
5. the tool used in the script is the geolift package created by meta, further explinations of model parameters can be found here.
6. https://facebookincubator.github.io/GeoLift/docs/intro 
