
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data","description":{"R":"the data as a data frame"}},{"name":".caller","type":"String","default":"lmer","hidden":true},{"name":".interface","type":"String","default":"jamovi","hidden":true},{"name":"dep","title":"Dependent Variable","type":"Variable","default":null,"suggested":["continuous","ordinal"],"permitted":["numeric"],"description":{"R":"a string naming the dependent variable from `data`; the variable must be numeric. Not needed if `formula` is used.\n"}},{"name":"factors","title":"Factors","type":"Variables","suggested":["nominal"],"permitted":["factor"],"default":null,"description":{"R":"a vector of strings naming the fixed factors from `data`. Not needed if `formula` is used."}},{"name":"covs","title":"Covariates","type":"Variables","suggested":["continuous","ordinal"],"permitted":["numeric"],"default":null,"description":{"R":"a vector of strings naming the covariates from `data`. Not needed if `formula` is used."}},{"name":"model_terms","title":"Model Terms","type":"Terms","default":null,"description":{"R":"a list of character vectors describing fixed effects terms. Not needed if `formula` is used.\n"}},{"name":"fixed_intercept","title":"Fixed Intercept","type":"Bool","default":true,"description":{"R":"`TRUE` (default) or `FALSE`, estimates fixed intercept. Not needed if `formula` is used.\n"}},{"name":"nested_terms","title":"Nested Model","type":"Terms","default":null,"description":{"R":"a list of character vectors describing effects terms for nestet. It can be passed as right-hand formula.\n"}},{"name":"comparison","title":"Activate","type":"Bool","default":false,"description":{"R":"Not present in R\n"}},{"name":"nested_intercept","title":"Nested Intercept","type":"Bool","default":true,"description":{"R":"`TRUE` (default) or `FALSE`, estimates fixed intercept. Not needed if `formula` is used.\n"}},{"name":"omnibus","title":"Test","type":"List","hidden":true,"default":"LRT","options":[{"name":"LRT","title":"LRT"}],"description":{"R":"`TRUE` (default) or `FALSE`, estimates fixed intercept. Not needed if `formula` is used.\n"}},{"name":"estimates_ci","title":"Coefficients","type":"Bool","default":true,"description":{"R":"`TRUE` (default) or `FALSE` , parameters CI in table\n"}},{"name":"donotrun","title":"Do not run","type":"Bool","default":false},{"name":"ci_method","title":"CI Method","type":"List","default":"wald","options":[{"name":"wald","title":"Standard (fast)"},{"name":"quantile","title":"Bootstrap (Percent)"},{"name":"bcai","title":"Bootstrap (BCa)"}]},{"name":"boot_r","title":"Bootstrap rep.","type":"Number","min":1,"default":1000,"description":{"R":"a number bootstrap repetitions.\n"}},{"name":"ci_width","title":"Confidence level","type":"Number","min":50,"max":99.9,"default":95,"description":{"R":"a number between 50 and 99.9 (default: 95) specifying the confidence interval width for the plots.\n"}},{"name":"contrasts","title":"Factors Coding","type":"Array","items":"(factors)","default":null,"template":{"type":"Group","elements":[{"name":"var","type":"Variable","content":"$key"},{"name":"type","type":"List","options":["simple","deviation","dummy","difference","helmert","repeated","polynomial"],"default":"simple"}]},"description":{"R":"a named vector of the form `c(var1=\"type\", var2=\"type2\")` specifying the type of contrast to use, one of `'deviation'`, `'simple'`, `'dummy'`, `'difference'`, `'helmert'`, `'repeated'` or `'polynomial'`. If NULL, `simple` is used. Can also be passed as a list of list of the form list(list(var=\"var1\",type=\"type1\")).\n"}},{"name":"show_contrastnames","title":"Names in estimates table","type":"Bool","default":true,"description":{"R":"`TRUE` or `FALSE` (default), shows raw names of the contrasts variables in tables\n"}},{"name":"show_contrastcodes","title":"Contrast Coefficients tables","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), shows contrast coefficients tables\n"}},{"name":"plotHAxis","title":"Horizontal axis","type":"Variable","default":null,"description":{"R":"a string naming the variable placed on the horizontal axis of the plot\n"}},{"name":"plotSepLines","title":"Separate lines","type":"Variable","default":null,"description":{"R":"a string naming the variable represented as separate lines in the plot\n"}},{"name":"plotSepPlots","title":"Separate plots","type":"Variables","default":null,"description":{"R":"a list of string naming the variables defining the levels for multiple plots\n"}},{"name":"plotRaw","title":"Observed scores","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), plot raw data along the predicted values\n"}},{"name":"plotDvScale","title":"Y-axis observed range","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), set the Y-axis range equal to the range of the observed values.\n"}},{"name":"plotOriginalScale","title":"Original scale","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), use original scale for covariates.\n"}},{"name":"plotLinesTypes","title":"Varying line types","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), use different linetypes per levels.\n"}},{"name":"plotError","title":"Error Bar Definition","type":"List","options":[{"name":"none","title":"None"},{"name":"ci","title":"Confidence intervals"},{"name":"se","title":"Standard Error"}],"default":"none","description":{"R":"`'none'` (default), `'ci'`, or `'se'`. Use no error bars, use confidence intervals, or use standard errors on the plots, respectively.\n"}},{"name":"emmeans","title":"Estimated Marginal Means","type":"Terms","default":null,"description":{"R":"a rhs formula with the terms specifying the marginal means to estimate (of the form `'~x+x:z'`)"}},{"name":"posthoc","title":"Post Hoc Tests","type":"Terms","default":null,"description":{"R":"a rhs formula with the terms specifying the table to apply the comparisons (of the form `'~x+x:z'`). The formula is not expanded, so '`x*z`' becomes '`x+z' and not '`x+z+x:z`'. It can be passed also as a list of the form '`list(\"x\",\"z\",c(\"x\",\"z\")`'"}},{"name":"simple_effects","title":"Simple effects variable","type":"Variable","default":null,"description":{"R":"The variable for which the simple effects (slopes) are computed\n"}},{"name":"simple_moderators","title":"Moderators","type":"Variables","default":null,"description":{"R":"the variable that provides the levels at which the simple effects are computed\n"}},{"name":"simple_interactions","title":"Simple Interactions","type":"Bool","default":false,"description":{"R":"should simple Interactions be computed\n"}},{"name":"covs_conditioning","title":"Covariates Conditioning","type":"List","options":[{"name":"mean_sd","title":"Mean ±  SD"},{"name":"percent","title":"Percentiles 50 ± offset"}],"default":"mean_sd","description":{"R":"`'mean_sd'` (default), `'custom'` , or `'percent'`. Use to condition the covariates (if any)\n"}},{"name":"ccm_value","type":"Number","default":1,"description":{"R":"how many st.deviations around the means used to condition simple effects and plots. Used if `simpleScale`=`'mean_sd'`\n"}},{"name":"ccp_value","type":"Number","default":25,"min":5,"max":50,"description":{"R":"offsett (number of percentiles) around the median used to condition simple effects and plots. Used if `simpleScale`=`'percent'`\n"}},{"name":"covs_scale_labels","type":"List","options":[{"name":"labels","title":"Labels"},{"name":"values","title":"Values"},{"name":"values_labels","title":"Values + Labels"},{"name":"uvalues","title":"Unscaled Values"},{"name":"uvalues_labels","title":"Unscaled Values + Labels"}],"default":"labels","description":{"R":"how the levels of a continuous moderator should appear in tables and plots: `labels`, `values` and `values_labels`, `ovalues`, `ovalues_labels. The latter two refer to the variable orginal levels, before scaling.\n"}},{"name":"adjust","title":"Correction","type":"NMXList","options":[{"name":"none","title":"No correction (LSD)"},{"name":"bonf","title":"Bonferroni"},{"name":"tukey","title":"Tukey"},{"name":"holm","title":"Holm"},{"name":"scheffe","title":"Scheffe"},{"name":"sidak","title":"Sidak"}],"default":["bonf"],"description":{"R":"one or more of `'none'`,  `'bonf'`,`'tukey'`  `'holm'`,`'scheffe'`, `'tukey'`; provide no,  Bonferroni, Tukey and Holm Post Hoc corrections respectively.\n"}},{"name":"predicted","title":"Predicted","type":"Output"},{"name":"residuals","title":"Residuals","type":"Output"},{"name":"modeltype","type":"List","hidden":true,"options":[{"name":"lmer"}],"default":"lmer"},{"name":"covs_scale","title":"Covariates Scaling","type":"Array","items":"(covs)","default":null,"template":{"type":"Group","elements":[{"name":"var","type":"Variable","content":"$key"},{"name":"type","type":"List","options":[{"title":"Centered","name":"centered"},{"title":"z-scores","name":"standardized"},{"title":"Centered clusterwise","name":"clusterbasedcentered"},{"title":"Clusters means","name":"clustermeans"},{"title":"z-scores clusterwise","name":"clusterbasedstandardized"},{"title":"None","name":"none"}],"default":"centered"}]},"description":{"R":"a list of lists specifying the covariates scaling, one of `'centered to the mean'`, `'standardized'`, or `'none'`. `'none'` leaves the variable as it is\n"}},{"name":"dep_scale","title":"Scale","type":"List","options":[{"title":"Original","name":"none"},{"title":"Centered","name":"centered"},{"title":"z-scores","name":"standardized"},{"title":"Centered clusterwise","name":"clusterbasedcentered"},{"title":"Clusters means","name":"clustermeans"},{"title":"z-scores clusterwise","name":"clusterbasedstandardized"}],"default":"none","description":{"R":"Re-scale the dependent variable.\n"}},{"name":"scale_missing","title":"Scale on","type":"List","options":[{"title":"Complete cases","name":"complete"},{"title":"Columnwise","name":"colwise"}]},{"name":"norm_test","title":"Normality of residuals","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), provide a test for normality of residuals\n"}},{"name":"cluster","title":"Cluster variables","type":"Variables","default":null,"suggested":["nominal"],"description":{"R":"a vector of strings naming the clustering variables from `data`"}},{"name":"re","title":"Random Coefficients","type":"Array","default":[[]],"template":{"type":"Terms"},"description":{"R":"a list of lists specifying the models random effects.          \n"}},{"name":"nested_re","title":"Nested Model Random Coefficients","type":"Array","default":[[]],"template":{"type":"Terms"},"description":{"R":"a list of lists specifying the models random effects.          \n"}},{"name":"re_corr","title":"Effects correlation","type":"List","options":[{"name":"all","title":"Correlated"},{"name":"none","title":"Not correlated"},{"name":"block","title":"Correlated by block"}],"default":"all","description":{"R":"`'all'`, `'none'` (default), or `'block'`. When random effects are passed as list of length 1, it decides whether the effects should be correlated,  non correlated. If `'re'` is a list of  lists of length > 1, the option is automatially set to `'block'`. The option is ignored if the model is passed using `formula`.\n"}},{"name":"re_modelterms","title":"Model terms","type":"Bool","default":true,"description":{"R":"Not in R interface\n"}},{"name":"re_listing","type":"List","title":"Add","options":[{"name":"none","title":"None"},{"name":"main","title":"Main effects"},{"name":"way2","title":"Up to 2-way"},{"name":"way3","title":"Up to 3-way"},{"name":"all","title":"All possible"}],"default":"none","description":{"R":"Not in R interface\n"}},{"name":"reml","title":"REML","type":"Bool","default":true,"description":{"R":"`TRUE` (default) or `FALSE`, should the Restricted ML be used rather than ML\n"}},{"name":"re_lrt","title":"LRT for Random Effects","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), LRT for the random effects\n"}},{"name":"re_ci","title":"Random Effect C.I. (slow)","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), confidence intervals for the random effects\n"}},{"name":"df_method","title":"DF Method","type":"List","default":"Satterthwaite","options":[{"name":"Satterthwaite","title":"Satterthwaite"},{"name":"Kenward-Roger","title":"Kenward-Roger  (slow)"}]},{"name":"norm_plot","title":"Histogram of residuals","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), provide a histogram of residuals superimposed by a normal distribution\n"}},{"name":"qq_plot","title":"Q-Q plot of residuals","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), provide a Q-Q plot of residuals\n"}},{"name":"resid_plot","title":"Residuals-Predicted scatterplot","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), provide a scatterplot of the residuals against predicted\n"}},{"name":"cluster_boxplot","title":"Residuals by cluster boxplot","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), provide a boxplot of random effects by the first defined clustering variable\n"}},{"name":"cluster_respred","title":"Residuals vs Predicted","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), residuals vs predicted by the first defined clustering variable\n"}},{"name":"rand_hist","title":"Histogram of random Coefficients","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), provide histogram of random Coefficients\n"}}];

const view = function() {
    
    

    View.extend({
        jus: "2.0",

        events: [

	],

	update: require('./mixed.events').update

    }).call(this);
}

view.layout = ui.extend({

    label: "Mixed Model",
    jus: "2.0",
    type: "root",
    stage: 0, //0 - release, 1 - development, 2 - proposed
    controls: [
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
								{ execute: require('./mixed.events').onChange_factors }
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
								{ execute: require('./mixed.events').onChange_covariates }
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
							name: "cluster",
							height: "small",
							isTarget: true,
							events: [
								{ execute: require('./mixed.events').onChange_cluster }
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
			style: "inline",
			controls: [
				{
					type: DefaultControls.Label,
					typeName: 'Label',
					label: "Estimation",
					style: "list",
					controls: [
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "reml"
						},
						{
							name: "donotrun",
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox'
						}
					]
				},
				{
					type: DefaultControls.Label,
					typeName: 'Label',
					label: "Confidence Intervals",
					margin: "large",
					style: "list",
					controls: [
						{
							name: "estimates_ci",
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							label: "Fixed parameters"
						},
						{
							name: "re_ci",
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							label: "Random variances"
						},
						{
							type: DefaultControls.TextBox,
							typeName: 'TextBox',
							name: "ci_width",
							format: FormatDef.number
						}
					]
				},
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
								{ execute: require('./mixed.events').onEvent_comparison }
							]
						}
					]
				}
			]
		},
		{
			type: DefaultControls.CollapseBox,
			typeName: 'CollapseBox',
			label: "Fixed Effects",
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
								},
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "fixed_intercept"
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
								},
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "nested_intercept",
									enable: "(comparison)"
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
			label: "Random Effects",
			collapsed: true,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.Supplier,
					typeName: 'Supplier',
					name: "randomSupplier",
					label: "Components",
					persistentItems: true,
					stretchFactor: 1,
					events: [
						{ execute: require('./mixed.events').onChange_randomSupplier },
						{ onEvent: 'update', execute: require('./mixed.events').onUpdate_randomSupplier }
					],
					controls: [
						{
							type: DefaultControls.TargetLayoutBox,
							typeName: 'TargetLayoutBox',
							transferAction: "none",
							name: "re_layout",
							controls: [
								{
									type: DefaultControls.ListBox,
									typeName: 'ListBox',
									name: "re",
									height: "normal",
									addButton: "Add correlated effects block",
									events: [
										{ execute: require('./mixed.events').onChange_nested_re_add }
									],
									selectable: true,
									templateName: "linreg-block-template",
									template:
									{
										type: DefaultControls.LayoutBox,
										typeName: 'LayoutBox',
										margin: "normal",
										targetArea: true,
										controls: [
											{
												type: DefaultControls.ListBox,
												typeName: 'ListBox',
												name: "randblockList",
												height: "auto",
												isTarget: true,
												valueFilter: "unique",
												ghostText: "drag term here",
												events: [
													{ execute: require('./mixed.events').onEvent_addRandomTerm }
												],
												template:
												{
													type: DefaultControls.TermLabel,
													typeName: 'TermLabel',
													format: require('./rtermFormat')
												}												
											}
										]
									}									
								}
							]
						},
						{
							type: DefaultControls.TargetLayoutBox,
							typeName: 'TargetLayoutBox',
							transferAction: "none",
							name: "nested_re_layout",
							controls: [
								{
									type: DefaultControls.ListBox,
									typeName: 'ListBox',
									name: "nested_re",
									height: "normal",
									addButton: "Add correlated effects block",
									events: [
										{ onEvent: 'listItemAdded', execute: require('./mixed.events').onEvent_nothing },
										{ onEvent: 'listItemRemoved', execute: require('./mixed.events').onEvent_nothing }
									],
									selectable: true,
									templateName: "linreg-block-template",
									template:
									{
										type: DefaultControls.LayoutBox,
										typeName: 'LayoutBox',
										margin: "normal",
										targetArea: true,
										controls: [
											{
												type: DefaultControls.ListBox,
												typeName: 'ListBox',
												name: "re_randblockList",
												height: "auto",
												isTarget: true,
												valueFilter: "unique",
												ghostText: "drag term here",
												itemDropBehaviour: "emptyspace",
												events: [
													{ execute: require('./mixed.events').onEvent_addRandomTerm }
												],
												template:
												{
													type: DefaultControls.TermLabel,
													typeName: 'TermLabel',
													format: require('./rtermFormat')
												}												
											}
										]
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
							label: "List components",
							controls: [
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "re_modelterms",
									events: [
										{ execute: require('./mixed.events').onEvent_re_list }
									]
								},
								{
									type: DefaultControls.ComboBox,
									typeName: 'ComboBox',
									name: "re_listing",
									events: [
										{ execute: require('./mixed.events').onEvent_re_list }
									]
								}
							]
						},
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "Effects correlation",
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
											name: "re_corr_all",
											optionName: "re_corr",
											optionPart: "all",
											events: [
												{ execute: require('./mixed.events').onEvent_corr }
											]
										},
										{
											type: DefaultControls.RadioButton,
											typeName: 'RadioButton',
											name: "re_corr_none",
											optionName: "re_corr",
											optionPart: "none"
										},
										{
											type: DefaultControls.RadioButton,
											typeName: 'RadioButton',
											name: "re_corr_block",
											optionName: "re_corr",
											optionPart: "block"
										}
									]
								}
							]
						},
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "Tests",
							controls: [
								{
									type: DefaultControls.LayoutBox,
									typeName: 'LayoutBox',
									margin: "large",
									controls: [
										{
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											name: "re_lrt"
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
							type: DefaultControls.LayoutBox,
							typeName: 'LayoutBox',
							margin: "small",
							style: "list",
							controls: [
								{
									type: DefaultControls.Label,
									typeName: 'Label',
									label: "Covariates conditioning",
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
													format: FormatDef.number,
													enable: "(covs_conditioning_mean_sd)"
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
								},
								{
									type: DefaultControls.Label,
									typeName: 'Label',
									label: "Dependent variable",
									controls: [
										{
											type: DefaultControls.ComboBox,
											typeName: 'ComboBox',
											name: "dep_scale"
										}
									]
								},
								{
									type: DefaultControls.Label,
									typeName: 'Label',
									label: "Scaling on",
									controls: [
										{
											type: DefaultControls.ComboBox,
											typeName: 'ComboBox',
											name: "scale_missing",
											label: ""
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
								},
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
							label: "Plot",
							controls: [
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "plotRaw"
								},
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "plotDvScale"
								}
							]
						},
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "Use",
							controls: [
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
					label: "Simple interactions"
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
			label: "Assumption Checks",
			collapsed: true,
			stretchFactor: 1,
			style: "inline",
			controls: [
				{
					type: DefaultControls.Label,
					typeName: 'Label',
					label: "Tests",
					controls: [
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "norm_test"
						}
					]
				},
				{
					type: DefaultControls.Label,
					typeName: 'Label',
					label: "Residual Plots",
					controls: [
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "qq_plot",
							label: "QQ plot"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "norm_plot",
							label: "histogram"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "resid_plot",
							label: "Residuals-Predicted"
						}
					]
				},
				{
					type: DefaultControls.Label,
					typeName: 'Label',
					label: "By clusters",
					controls: [
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "cluster_boxplot",
							label: "Residuals boxplot"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "cluster_respred",
							label: "Residuals-Predicted"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "rand_hist",
							label: "Random coeff. histogram"
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
									name: "ci_method_quantile",
									optionName: "ci_method",
									optionPart: "quantile"
								},
								{
									type: DefaultControls.RadioButton,
									typeName: 'RadioButton',
									name: "ci_method_bcai",
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
							label: "DF Method",
							controls: [
								{
									type: DefaultControls.RadioButton,
									typeName: 'RadioButton',
									name: "df_method_Satterthwaite",
									optionName: "df_method",
									optionPart: "Satterthwaite"
								},
								{
									name: "df_method_Kenward-Roger",
									type: DefaultControls.RadioButton,
									typeName: 'RadioButton',
									optionName: "df_method",
									optionPart: "Kenward-Roger"
								}
							]
						},
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
