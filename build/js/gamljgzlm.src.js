
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data","description":{"R":"the data as a data frame"}},{"name":".caller","type":"String","default":"glm","hidden":true},{"name":".interface","type":"String","default":"jamovi","hidden":true},{"name":"dep","title":"Dependent Variable","type":"Variable","default":null,"permitted":["factor","numeric"],"description":{"R":"a string naming the dependent variable from `data`; the variable must be numeric. Not needed if `formula` is used.\n"}},{"name":"factors","title":"Factors","type":"Variables","suggested":["nominal"],"permitted":["factor"],"default":null,"description":{"R":"a vector of strings naming the fixed factors from `data`. Not needed if `formula` is used."}},{"name":"covs","title":"Covariates","type":"Variables","suggested":["continuous","ordinal"],"permitted":["numeric"],"default":null,"description":{"R":"a vector of strings naming the covariates from `data`. Not needed if `formula` is used."}},{"name":"model_terms","title":"Model Terms","type":"Terms","default":null,"description":{"R":"a list of character vectors describing fixed effects terms. Not needed if `formula` is used.\n"}},{"name":"fixed_intercept","title":"Intercept","type":"Bool","default":true,"description":{"R":"`TRUE` (default) or `FALSE`, estimates fixed intercept. Not needed if `formula` is used.\n"}},{"name":"nested_intercept","title":"Intercept","type":"Bool","default":true,"description":{"R":"`TRUE` (default) or `FALSE`, estimates fixed intercept. Not needed if `formula` is used.\n"}},{"name":"nested_terms","title":"Nested Model","type":"Terms","default":null,"description":{"R":"a list of character vectors describing effects terms for nestet. It can be passed as right-hand formula.\n"}},{"name":"comparison","title":"Activate","type":"Bool","default":false,"description":{"R":"Not present in R\n"}},{"name":"omnibus","title":"Test","type":"List","default":"LRT","options":[{"name":"wald","title":"Wald Chi²"},{"name":"LRT","title":"LRT"}]},{"name":"estimates_ci","title":"For estimates","type":"Bool","default":false,"description":{"R":"`TRUE` (default) or `FALSE` , coefficients CI in tables\n"}},{"name":"donotrun","title":"Do not run","type":"Bool","default":false},{"name":"ci_method","title":"CI Method","type":"List","default":"wald","options":[{"name":"wald","title":"Standard"},{"name":"profile","title":"Profile"},{"name":"quantile","title":"Bootstrap Percent"},{"name":"bcai","title":"Bootstrap BCa"}]},{"name":"boot_r","title":"Bootstrap rep.","type":"Number","min":1,"default":1000,"description":{"R":"a number bootstrap repetitions.\n"}},{"name":"ci_width","title":"Confidence level","type":"Number","min":50,"max":99.9,"default":95,"description":{"R":"a number between 50 and 99.9 (default: 95) specifying the confidence interval width for the plots.\n"}},{"name":"contrasts","title":"Factors Coding","type":"Array","items":"(factors)","default":null,"template":{"type":"Group","elements":[{"name":"var","type":"Variable","content":"$key"},{"name":"type","type":"List","options":["simple","deviation","dummy","difference","helmert","repeated","polynomial"],"default":"simple"}]},"description":{"R":"a named vector of the form `c(var1=\"type\", var2=\"type2\")` specifying the type of contrast to use, one of `'deviation'`, `'simple'`, `'dummy'`, `'difference'`, `'helmert'`, `'repeated'` or `'polynomial'`. If NULL, `simple` is used. Can also be passed as a list of list of the form list(list(var=\"var1\",type=\"type1\")).\n"}},{"name":"show_contrastnames","title":"Names in estimates table","type":"Bool","default":true,"description":{"R":"`TRUE` or `FALSE` (default), shows raw names of the contrasts variables in tables\n"}},{"name":"show_contrastcodes","title":"Contrast Coefficients tables","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), shows contrast coefficients tables\n"}},{"name":"vcov","title":"Coefficients Covariances","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), shows coefficients covariances\n"}},{"name":"plotHAxis","title":"Horizontal axis","type":"Variable","default":null,"description":{"R":"a string naming the variable placed on the horizontal axis of the plot\n"}},{"name":"plotSepLines","title":"Separate lines","type":"Variable","default":null,"description":{"R":"a string naming the variable represented as separate lines in the plot\n"}},{"name":"plotSepPlots","title":"Separate plots","type":"Variables","default":null,"description":{"R":"a list of string naming the variables defining the levels for multiple plots\n"}},{"name":"plotRaw","title":"Observed scores","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), plot raw data along the predicted values\n"}},{"name":"plotDvScale","title":"Y-axis observed range","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), set the Y-axis range equal to the range of the observed values.\n"}},{"name":"plotOriginalScale","title":"Original scale","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), use original scale for covariates.\n"}},{"name":"plotLinesTypes","title":"Varying line types","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), use different linetypes per levels.\n"}},{"name":"plotError","title":"Error Bar Definition","type":"List","options":[{"name":"none","title":"None"},{"name":"ci","title":"Confidence intervals"},{"name":"se","title":"Standard Error"}],"default":"none","description":{"R":"`'none'` (default), `'ci'`, or `'se'`. Use no error bars, use confidence intervals, or use standard errors on the plots, respectively.\n"}},{"name":"emmeans","title":"Estimated Marginal Means","type":"Terms","default":null,"description":{"R":"a rhs formula with the terms specifying the marginal means to estimate (of the form `'~x+x:z'`)"}},{"name":"posthoc","title":"Post Hoc Tests","type":"Terms","default":null,"description":{"R":"a rhs formula with the terms specifying the table to apply the comparisons (of the form `'~x+x:z'`). The formula is not expanded, so '`x*z`' becomes '`x+z' and not '`x+z+x:z`'. It can be passed also as a list of the form '`list(\"x\",\"z\",c(\"x\",\"z\")`'"}},{"name":"simple_effects","title":"Simple effects variable","type":"Variable","default":null,"description":{"R":"The variable for which the simple effects (slopes) are computed\n"}},{"name":"simple_moderators","title":"Moderators","type":"Variables","default":null,"description":{"R":"the variable that provides the levels at which the simple effects are computed\n"}},{"name":"simple_interactions","title":"Simple Interactions","type":"Bool","default":false,"description":{"R":"should simple Interactions be computed\n"}},{"name":"covs_conditioning","title":"Covariates conditioning","type":"List","options":[{"name":"mean_sd","title":"Mean ±  SD"},{"name":"percent","title":"Percentiles 50 ± offset"}],"default":"mean_sd","description":{"R":"`'mean_sd'` (default), `'custom'` , or `'percent'`. Use to condition the covariates (if any)\n"}},{"name":"ccm_value","type":"Number","default":1,"description":{"R":"how many st.deviations around the means used to condition simple effects and plots. Used if `simpleScale`=`'mean_sd'`\n"}},{"name":"ccp_value","type":"Number","default":25,"min":5,"max":50,"description":{"R":"offsett (number of percentiles) around the median used to condition simple effects and plots. Used if `simpleScale`=`'percent'`\n"}},{"name":"covs_scale_labels","type":"List","options":[{"name":"labels","title":"Labels"},{"name":"values","title":"Values"},{"name":"values_labels","title":"Values + Labels"},{"name":"uvalues","title":"Unscaled Values"},{"name":"uvalues_labels","title":"Unscaled Values + Labels"}],"default":"labels","description":{"R":"how the levels of a continuous moderator should appear in tables and plots: `labels`, `values` and `values_labels`, `ovalues`, `ovalues_labels. The latter two refer to the variable orginal levels, before scaling.\n"}},{"name":"adjust","title":"Correction","type":"NMXList","options":[{"name":"none","title":"No correction (LSD)"},{"name":"bonf","title":"Bonferroni"},{"name":"tukey","title":"Tukey"},{"name":"holm","title":"Holm"},{"name":"scheffe","title":"Scheffe"},{"name":"sidak","title":"Sidak"}],"default":["bonf"],"description":{"R":"one or more of `'none'`,  `'bonf'`,`'tukey'`  `'holm'`; provide no,  Bonferroni, Tukey and Holm Post Hoc corrections respectively.\n"}},{"name":"predicted","title":"Predicted","type":"Output"},{"name":"residuals","title":"Residuals","type":"Output"},{"name":"covs_scale","title":"Covariates Scaling","type":"Array","items":"(covs)","default":null,"template":{"type":"Group","elements":[{"name":"var","type":"Variable","content":"$key"},{"name":"type","type":"List","options":[{"title":"Centered","name":"centered"},{"title":"z-scores","name":"standardized"},{"title":"None","name":"none"}],"default":"centered"}]},"description":{"R":"a named vector of the form \\code{c(var1='type', var2='type2')} specifying the transformation to apply to covariates, one of `'centered'` to the mean, `'standardized'`,`'log'` or  `'none'`. `'none'` leaves the variable as it is.\n"}},{"name":"expb_ci","title":"For exp(B)","type":"Bool","default":true,"description":{"R":"`TRUE` (default) or `FALSE` , exp(B) CI in table\n"}},{"name":"es","title":"Effect Size","type":"NMXList","options":[{"name":"expb","title":"Odd Ratios (expB)"},{"name":"RR","title":"Relative Risk (RR)"}],"default":["expb"]},{"name":"modeltype","title":"Model Selection","type":"List","options":[{"name":"linear","title":"Linear"},{"name":"poisson","title":"Poisson"},{"name":"poiover","title":"Poisson (overdispersion)"},{"name":"nb","title":"Negative Binomial"},{"name":"logistic","title":"Logistic"},{"name":"probit","title":"Probit"},{"name":"custom","title":"Custom"},{"name":"ordinal","title":"Ordinal (proportional odds)"},{"name":"multinomial","title":"Multinomial"}],"default":"linear","description":{"R":"Select the generalized linear model: `linear`,`poisson`,`logistic`,`multinomial`\n"}},{"name":"custom_family","title":"Distribution","type":"List","options":[{"title":"Gaussian","name":"gaussian"},{"title":"Binomial","name":"binomial"},{"title":"Poisson","name":"poisson"},{"title":"Inverse gaussian","name":"inverse.gaussian"},{"title":"Gamma","name":"Gamma"}],"default":"gaussian","description":{"R":"Distribution family for the custom model, accepts gaussian, binomial, gamma and inverse_gaussian .\n"}},{"name":"custom_link","title":"Link Function","type":"List","options":[{"title":"Identity","name":"identity"},{"title":"Logit","name":"logit"},{"title":"Log","name":"log"},{"title":"Inverse","name":"inverse"},{"title":"Inverse squared","name":"1/mu^2"},{"title":"Square root","name":"sqrt"}],"default":"identity","description":{"R":"Distribution family for the custom model, accepts  identity, log and inverse, onemu2 (for 1/mu^2).\n"}},{"name":"propodds_test","title":"Parallel lines test","type":"Bool","default":false,"description":{"R":"Test parallel lines assumptions in cumulative link model (ordinal regression)\n"}},{"name":"plot_scale","title":"Y-axis scale","type":"List","options":[{"name":"response","title":"Response"},{"name":"link","title":"Linear predictor"},{"name":"mean.class","title":"Mean class"}],"default":"response","description":{"R":"Chi-squared computation method. `'lrt'` (default) gives LogLikelihood ration test,  `'wald'` gives the Wald Chi-squared.\n"}}];

const view = function() {
    
    

    View.extend({
        jus: "2.0",

        events: [

	],

	update: require('./gamlj.events').update

    }).call(this);
}

view.layout = ui.extend({

    label: "Generalized Linear Models",
    jus: "2.0",
    type: "root",
    stage: 0, //0 - release, 1 - development, 2 - proposed
    controls: [
		{
			type: DefaultControls.LayoutBox,
			typeName: 'LayoutBox',
			margin: "large",
			style: "inline",
			controls: [
				{
					type: DefaultControls.LayoutBox,
					typeName: 'LayoutBox',
					margin: "large",
					controls: [
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "Continuous dependent variable",
							controls: [
								{
									type: DefaultControls.RadioButton,
									typeName: 'RadioButton',
									name: "modeltype_linear",
									optionName: "modeltype",
									optionPart: "linear",
									events: [
										{ execute: require('./gamlj.events').onChange_model }
									]
								}
							]
						},
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "Frequencies",
							controls: [
								{
									type: DefaultControls.RadioButton,
									typeName: 'RadioButton',
									name: "modeltype_poisson",
									optionName: "modeltype",
									optionPart: "poisson",
									events: [
										{ execute: require('./gamlj.events').onChange_model }
									]
								},
								{
									name: "modeltype_poiover",
									type: DefaultControls.RadioButton,
									typeName: 'RadioButton',
									optionName: "modeltype",
									optionPart: "poiover",
									events: [
										{ execute: require('./gamlj.events').onChange_model }
									]
								},
								{
									type: DefaultControls.RadioButton,
									typeName: 'RadioButton',
									name: "modeltype_nb",
									optionName: "modeltype",
									optionPart: "nb",
									events: [
										{ execute: require('./gamlj.events').onChange_model }
									]
								}
							]
						}
					]
				},
				{
					type: DefaultControls.LayoutBox,
					typeName: 'LayoutBox',
					margin: "large",
					controls: [
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "Categorical dependent variable",
							controls: [
								{
									type: DefaultControls.RadioButton,
									typeName: 'RadioButton',
									name: "modeltype_logistic",
									optionName: "modeltype",
									optionPart: "logistic",
									events: [
										{ execute: require('./gamlj.events').onChange_model }
									]
								},
								{
									type: DefaultControls.RadioButton,
									typeName: 'RadioButton',
									name: "modeltype_probit",
									optionName: "modeltype",
									optionPart: "probit",
									events: [
										{ execute: require('./gamlj.events').onChange_model }
									]
								},
								{
									name: "modeltype_ordinal",
									type: DefaultControls.RadioButton,
									typeName: 'RadioButton',
									optionName: "modeltype",
									optionPart: "ordinal",
									events: [
										{ execute: require('./gamlj.events').onChange_model }
									]
								},
								{
									type: DefaultControls.RadioButton,
									typeName: 'RadioButton',
									name: "modeltype_multinomial",
									optionName: "modeltype",
									optionPart: "multinomial",
									events: [
										{ execute: require('./gamlj.events').onChange_model }
									]
								}
							]
						},
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "Custom Model",
							controls: [
								{
									type: DefaultControls.RadioButton,
									typeName: 'RadioButton',
									name: "modeltype_custom",
									optionName: "modeltype",
									optionPart: "custom",
									events: [
										{ execute: require('./gamlj.events').onChange_model }
									]
								},
								{
									type: DefaultControls.ComboBox,
									typeName: 'ComboBox',
									name: "custom_family",
									enable: "(modeltype_custom)"
								},
								{
									type: DefaultControls.ComboBox,
									typeName: 'ComboBox',
									name: "custom_link",
									enable: "(modeltype_custom)",
									events: [
										{ execute: require('./gamlj.events').onChange_model }
									]
								}
							]
						}
					]
				}
			]
		},
		{
			type: DefaultControls.VariableSupplier,
			typeName: 'VariableSupplier',
			name: "variablesupplier",
			suggested: ["continuous","nominal","ordinal"],
			persistentItems: false,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "dep",
							maxItemCount: 1,
							isTarget: true,
							itemDropBehaviour: "overwrite"
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "factors",
							isTarget: true,
							events: [
								{ execute: require('./gamlj.events').onChange_factors }
							]
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "covs",
							isTarget: true,
							events: [
								{ execute: require('./gamlj.events').onChange_covariates }
							]
						}
					]
				}
			]
		},
		{
			type: DefaultControls.LayoutBox,
			typeName: 'LayoutBox',
			margin: "large",
			style: "list",
			controls: [
				{
					type: DefaultControls.Label,
					typeName: 'Label',
					label: "Effect Size",
					margin: "large",
					style: "list-inline",
					controls: [
						{
							name: "es_expb",
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							optionPart: "expb",
							optionName: "es"
						},
						{
							name: "es_RR",
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							optionPart: "RR",
							optionName: "es",
							enable: "(modeltype:logistic)"
						}
					]
				},
				{
					type: DefaultControls.Label,
					typeName: 'Label',
					label: "Confidence Intervals",
					margin: "large",
					style: "list-inline",
					controls: [
						{
							name: "expb_ci",
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox'
						},
						{
							name: "estimates_ci",
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox'
						},
						{
							type: DefaultControls.TextBox,
							typeName: 'TextBox',
							name: "ci_width",
							label: "Interval",
							suffix: "%",
							format: FormatDef.number,
							enable: "(estimates_ci || expb_ci)"
						}
					]
				},
				{
					type: DefaultControls.Label,
					typeName: 'Label',
					label: "Estimation",
					margin: "large",
					style: "list-inline",
					controls: [
						{
							name: "donotrun",
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox'
						}
					]
				}
			]
		},
		{
			type: DefaultControls.CollapseBox,
			typeName: 'CollapseBox',
			label: "Model",
			collapsed: true,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.Supplier,
					typeName: 'Supplier',
					name: "modelSupplier",
					label: "Components",
					persistentItems: true,
					stretchFactor: 1,
					format: FormatDef.term,
					higherOrders: true,
					events: [
						{ onEvent: 'update', execute: require('./gamlj.events').onUpdate_modelSupplier }
					],
					controls: [
						{
							type: DefaultControls.TargetLayoutBox,
							typeName: 'TargetLayoutBox',
							transferAction: "interactions",
							controls: [
								{
									type: DefaultControls.ListBox,
									typeName: 'ListBox',
									name: "model_terms",
									valueFilter: "unique",
									isTarget: true,
									itemDropBehaviour: "emptyspace",
									events: [
										{ execute: require('./gamlj.events').onChange_model_terms },
										{ onEvent: 'listItemRemoved', execute: require('./gamlj.events').onChange_model_remove }
									],
									template:
									{
										type: DefaultControls.TermLabel,
										typeName: 'TermLabel'
									}									
								}
							]
						},
						{
							type: DefaultControls.TargetLayoutBox,
							typeName: 'TargetLayoutBox',
							name: "nested_layout",
							transferAction: "interactions",
							controls: [
								{
									type: DefaultControls.ListBox,
									typeName: 'ListBox',
									name: "nested_terms",
									valueFilter: "unique",
									isTarget: true,
									itemDropBehaviour: "emptyspace",
									events: [
										{ execute: require('./gamlj.events').onChange_nested_add }
									],
									template:
									{
										type: DefaultControls.TermLabel,
										typeName: 'TermLabel'
									}									
								}
							]
						}
					]
				},
				{
					type: DefaultControls.LayoutBox,
					typeName: 'LayoutBox',
					style: "inline",
					controls: [
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "Model comparison",
							controls: [
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "comparison",
									events: [
										{ execute: require('./gamlj.events').onEvent_comparison }
									]
								},
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "nested_intercept",
									enable: "(comparison)"
								}
							]
						},
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "Parameters",
							controls: [
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "fixed_intercept"
								}
							]
						},
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "Omnibus test",
							controls: [
								{
									name: "omnibus_X",
									type: DefaultControls.RadioButton,
									typeName: 'RadioButton',
									optionName: "omnibus",
									optionPart: "wald"
								},
								{
									name: "omnibus_LRT",
									type: DefaultControls.RadioButton,
									typeName: 'RadioButton',
									optionName: "omnibus",
									optionPart: "LRT"
								}
							]
						}
					]
				}
			]
		},
		{
			type: DefaultControls.CollapseBox,
			typeName: 'CollapseBox',
			label: "Factors Coding",
			collapsed: true,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.ListBox,
					typeName: 'ListBox',
					name: "contrasts",
					stretchFactor: 1,
					showColumnHeaders: false,
					columns: [
						{
							name: "var",
							label: null,
							selectable: false,
							stretchFactor: 1,
							maxWidth: 300,
							template:
							{
								type: DefaultControls.VariableLabel,
								typeName: 'VariableLabel'
							}							
						},
						{
							name: "type",
							label: null,
							selectable: false,
							stretchFactor: 0.5,
							template:
							{
								type: DefaultControls.ComboBox,
								typeName: 'ComboBox'
							}							
						}
					]
				},
				{
					type: DefaultControls.CheckBox,
					typeName: 'CheckBox',
					name: "show_contrastnames"
				},
				{
					type: DefaultControls.CheckBox,
					typeName: 'CheckBox',
					name: "show_contrastcodes"
				}
			]
		},
		{
			type: DefaultControls.CollapseBox,
			typeName: 'CollapseBox',
			label: "Covariates Scaling",
			collapsed: true,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.ListBox,
					typeName: 'ListBox',
					name: "covs_scale",
					stretchFactor: 1,
					showColumnHeaders: false,
					columns: [
						{
							name: "var",
							label: null,
							selectable: false,
							stretchFactor: 1,
							maxWidth: 300,
							template:
							{
								type: DefaultControls.VariableLabel,
								typeName: 'VariableLabel'
							}							
						},
						{
							name: "type",
							label: null,
							selectable: false,
							stretchFactor: 0.5,
							template:
							{
								type: DefaultControls.ComboBox,
								typeName: 'ComboBox'
							}							
						}
					]
				},
				{
					type: DefaultControls.LayoutBox,
					typeName: 'LayoutBox',
					margin: "large",
					style: "inline",
					controls: [
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "Covariates conditioning",
							controls: [
								{
									type: DefaultControls.LayoutBox,
									typeName: 'LayoutBox',
									margin: "large",
									style: "list",
									controls: [
										{
											type: DefaultControls.RadioButton,
											typeName: 'RadioButton',
											name: "covs_conditioning_mean_sd",
											optionName: "covs_conditioning",
											optionPart: "mean_sd",
											controls: [
												{
													type: DefaultControls.TextBox,
													typeName: 'TextBox',
													name: "ccm_value",
													format: FormatDef.number
												}
											]
										},
										{
											name: "covs_conditioning_percent",
											type: DefaultControls.RadioButton,
											typeName: 'RadioButton',
											optionName: "covs_conditioning",
											optionPart: "percent",
											controls: [
												{
													type: DefaultControls.TextBox,
													typeName: 'TextBox',
													name: "ccp_value",
													label: null,
													suffix: "%",
													format: FormatDef.number,
													enable: "(covs_conditioning_percent)"
												}
											]
										}
									]
								}
							]
						},
						{
							type: DefaultControls.LayoutBox,
							typeName: 'LayoutBox',
							margin: "large",
							style: "list",
							controls: [
								{
									type: DefaultControls.Label,
									typeName: 'Label',
									label: "Covariates labeling",
									controls: [
										{
											type: DefaultControls.ComboBox,
											typeName: 'ComboBox',
											name: "covs_scale_labels"
										}
									]
								}
							]
						}
					]
				}
			]
		},
		{
			type: DefaultControls.CollapseBox,
			typeName: 'CollapseBox',
			label: "Post Hoc Tests",
			collapsed: true,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.Supplier,
					typeName: 'Supplier',
					name: "posthocSupplier",
					persistentItems: false,
					stretchFactor: 1,
					format: FormatDef.term,
					events: [
						{ execute: require('./gamlj.events').onChange_posthocSupplier },
						{ onEvent: 'update', execute: require('./gamlj.events').onUpdate_posthocSupplier }
					],
					controls: [
						{
							type: DefaultControls.TargetLayoutBox,
							typeName: 'TargetLayoutBox',
							label: "",
							controls: [
								{
									type: DefaultControls.ListBox,
									typeName: 'ListBox',
									name: "posthoc",
									isTarget: true,
									template:
									{
										type: DefaultControls.TermLabel,
										typeName: 'TermLabel'
									}									
								}
							]
						}
					]
				},
				{
					type: DefaultControls.LayoutBox,
					typeName: 'LayoutBox',
					margin: "large",
					style: "inline",
					controls: [
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "Correction",
							controls: [
								{
									type: DefaultControls.LayoutBox,
									typeName: 'LayoutBox',
									margin: "small",
									style: "list",
									controls: [
										{
											name: "adjust_none",
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											optionPart: "none",
											optionName: "adjust"
										},
										{
											name: "adjust_bonf",
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											optionPart: "bonf",
											optionName: "adjust"
										},
										{
											name: "adjust_tukey",
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											optionPart: "tukey",
											optionName: "adjust"
										}
									]
								},
								{
									type: DefaultControls.LayoutBox,
									typeName: 'LayoutBox',
									margin: "small",
									style: "list",
									controls: [
										{
											name: "adjust_holm",
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											optionPart: "holm",
											optionName: "adjust"
										},
										{
											name: "adjust_scheffe",
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											optionName: "adjust",
											optionPart: "scheffe"
										},
										{
											name: "adjust_sidak",
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											optionName: "adjust",
											optionPart: "sidak"
										}
									]
								}
							]
						}
					]
				}
			]
		},
		{
			type: DefaultControls.CollapseBox,
			typeName: 'CollapseBox',
			label: "Plots",
			collapsed: true,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.VariableSupplier,
					typeName: 'VariableSupplier',
					name: "plotsSupplier",
					populate: "manual",
					stretchFactor: 1,
					persistentItems: false,
					events: [
						{ onEvent: 'update', execute: require('./gamlj.events').onUpdate_plotsSupplier },
						{ execute: require('./gamlj.events').onChange_plotsSupplier }
					],
					controls: [
						{
							type: DefaultControls.TargetLayoutBox,
							typeName: 'TargetLayoutBox',
							controls: [
								{
									type: DefaultControls.VariablesListBox,
									typeName: 'VariablesListBox',
									name: "plotHAxis",
									isTarget: true,
									maxItemCount: 1
								}
							]
						},
						{
							type: DefaultControls.TargetLayoutBox,
							typeName: 'TargetLayoutBox',
							controls: [
								{
									type: DefaultControls.VariablesListBox,
									typeName: 'VariablesListBox',
									name: "plotSepLines",
									isTarget: true,
									maxItemCount: 1
								}
							]
						},
						{
							type: DefaultControls.TargetLayoutBox,
							typeName: 'TargetLayoutBox',
							controls: [
								{
									type: DefaultControls.VariablesListBox,
									typeName: 'VariablesListBox',
									name: "plotSepPlots",
									isTarget: true
								}
							]
						}
					]
				},
				{
					type: DefaultControls.LayoutBox,
					typeName: 'LayoutBox',
					margin: "large",
					style: "inline",
					controls: [
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "Display",
							controls: [
								{
									type: DefaultControls.RadioButton,
									typeName: 'RadioButton',
									name: "errBarDef_none",
									optionName: "plotError",
									optionPart: "none"
								},
								{
									type: DefaultControls.RadioButton,
									typeName: 'RadioButton',
									name: "errBarDef_ci",
									optionName: "plotError",
									optionPart: "ci"
								},
								{
									name: "plotError_se",
									type: DefaultControls.RadioButton,
									typeName: 'RadioButton',
									optionName: "plotError",
									optionPart: "se"
								}
							]
						},
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "Plot type",
							controls: [
								{
									type: DefaultControls.RadioButton,
									typeName: 'RadioButton',
									name: "plot_scale_response",
									optionName: "plot_scale",
									optionPart: "response"
								},
								{
									type: DefaultControls.RadioButton,
									typeName: 'RadioButton',
									name: "plot_scale_link",
									optionName: "plot_scale",
									optionPart: "link",
									enable: "(!modeltype_multinomial && !modeltype_ordinal)"
								},
								{
									type: DefaultControls.RadioButton,
									typeName: 'RadioButton',
									name: "plotcale_mean.class",
									optionName: "plot_scale",
									optionPart: "mean.class",
									enable: "(modeltype_ordinal)"
								}
							]
						},
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "Plot options",
							controls: [
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "plotRaw",
									enable: "(!modeltype_multinomial && !plot_scale_link)"
								},
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "plotDvScale",
									enable: "(plot_scale_response)"
								},
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "plotOriginalScale"
								},
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "plotLinesTypes"
								}
							]
						}
					]
				}
			]
		},
		{
			type: DefaultControls.CollapseBox,
			typeName: 'CollapseBox',
			label: "Simple Effects",
			collapsed: true,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.VariableSupplier,
					typeName: 'VariableSupplier',
					name: "simpleSupplier",
					populate: "manual",
					stretchFactor: 1,
					persistentItems: false,
					events: [
						{ execute: require('./gamlj.events').onChange_simpleSupplier },
						{ onEvent: 'update', execute: require('./gamlj.events').onUpdate_simpleSupplier }
					],
					controls: [
						{
							type: DefaultControls.TargetLayoutBox,
							typeName: 'TargetLayoutBox',
							controls: [
								{
									type: DefaultControls.VariablesListBox,
									typeName: 'VariablesListBox',
									name: "simple_effects",
									isTarget: true,
									maxItemCount: 1
								}
							]
						},
						{
							type: DefaultControls.TargetLayoutBox,
							typeName: 'TargetLayoutBox',
							controls: [
								{
									type: DefaultControls.VariablesListBox,
									typeName: 'VariablesListBox',
									name: "simple_moderators",
									isTarget: true
								}
							]
						}
					]
				},
				{
					type: DefaultControls.CheckBox,
					typeName: 'CheckBox',
					name: "simple_interactions",
					label: "Simple interactions",
					enable: "(!modeltype_multinomial)"
				}
			]
		},
		{
			type: DefaultControls.CollapseBox,
			typeName: 'CollapseBox',
			label: "Estimated Marginal Means",
			collapsed: true,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.Supplier,
					typeName: 'Supplier',
					name: "emmeansSupplier",
					persistentItems: false,
					stretchFactor: 1,
					format: FormatDef.term,
					events: [
						{ execute: require('./gamlj.events').onChange_emmeansSupplier },
						{ onEvent: 'update', execute: require('./gamlj.events').onUpdate_emmeansSupplier }
					],
					controls: [
						{
							type: DefaultControls.TargetLayoutBox,
							typeName: 'TargetLayoutBox',
							label: "",
							controls: [
								{
									type: DefaultControls.ListBox,
									typeName: 'ListBox',
									name: "emmeans",
									isTarget: true,
									template:
									{
										type: DefaultControls.TermLabel,
										typeName: 'TermLabel'
									}									
								}
							]
						}
					]
				}
			]
		},
		{
			type: DefaultControls.CollapseBox,
			typeName: 'CollapseBox',
			label: "Options",
			style: "list",
			collapsed: true,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.LayoutBox,
					typeName: 'LayoutBox',
					margin: "large",
					style: "inline",
					controls: [
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "CI Method",
							controls: [
								{
									type: DefaultControls.RadioButton,
									typeName: 'RadioButton',
									name: "ci_method_wald",
									optionName: "ci_method",
									optionPart: "wald"
								},
								{
									type: DefaultControls.RadioButton,
									typeName: 'RadioButton',
									name: "ci_method_profile",
									optionName: "ci_method",
									optionPart: "profile"
								},
								{
									name: "ci_method_quantile",
									type: DefaultControls.RadioButton,
									typeName: 'RadioButton',
									optionName: "ci_method",
									optionPart: "quantile"
								},
								{
									name: "ci_method_bcai",
									type: DefaultControls.RadioButton,
									typeName: 'RadioButton',
									optionName: "ci_method",
									optionPart: "bcai"
								},
								{
									type: DefaultControls.TextBox,
									typeName: 'TextBox',
									name: "boot_r",
									format: FormatDef.number,
									enable: "(ci_method_quantile || ci_method_bcai)"
								}
							]
						},
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							name: "add_info",
							label: "Additional info",
							controls: [
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "vcov"
								},
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "propodds_test",
									enable: "(modeltype_ordinal)"
								}
							]
						}
					]
				},
				{
					type: DefaultControls.LayoutBox,
					typeName: 'LayoutBox',
					margin: "large",
					style: "inline",
					controls: [
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "Save",
							controls: [
								{
									type: DefaultControls.Output,
									typeName: 'Output',
									name: "predicted"
								},
								{
									type: DefaultControls.Output,
									typeName: 'Output',
									name: "residuals"
								}
							]
						}
					]
				}
			]
		}
	]
});

module.exports = { view : view, options: options };
