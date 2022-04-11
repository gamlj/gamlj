
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data","description":{"R":"the data as a data frame"}},{"name":"caller","type":"String","default":"lmer","hidden":true},{"name":"modelSelection","type":"List","hidden":true,"options":[{"name":"lmer"}],"default":"lmer"},{"name":"dep","title":"Dependent Variable","type":"Variable","default":null,"permitted":["numeric"],"description":{"R":"a string naming the dependent variable from `data`, variable must be numeric\n"}},{"name":"factors","title":"Factors","type":"Variables","permitted":["factor"],"default":null,"description":{"R":"a vector of strings naming the fixed factors from `data`"}},{"name":"covs","title":"Covariates","type":"Variables","permitted":["numeric"],"default":null,"description":{"R":"a vector of strings naming the covariates from `data`"}},{"name":"modelTerms","title":"Model Terms","type":"Terms","default":null,"description":{"R":"a list of character vectors describing fixed effects terms\n"}},{"name":"fixedIntercept","title":"Fixed Intercept","type":"Bool","default":true,"description":{"R":"`TRUE` (default) or `FALSE`, estimates fixed intercept\n"}},{"name":"showParamsCI","title":"Confidence intervals","type":"Bool","default":true,"description":{"R":"`TRUE` (default) or `FALSE` , parameters CI in table\n"}},{"name":"paramCIWidth","title":"Confidence level","type":"Number","min":50,"max":99.9,"default":95,"description":{"R":"a number between 50 and 99.9 (default: 95) specifying the confidence interval width for the parameter estimates\n"}},{"name":"contrasts","title":"Factors Coding","type":"Array","items":"(factors)","default":null,"template":{"type":"Group","elements":[{"name":"var","type":"Variable","content":"$key"},{"name":"type","type":"List","options":["simple","deviation","dummy","difference","helmert","repeated","polynomial"],"default":"simple"}]},"description":{"R":"a list of lists specifying the factor and type of contrast to use, one of `'deviation'`, `'simple'`, `'difference'`, `'helmert'`, `'repeated'` or `'polynomial'`\n"}},{"name":"showRealNames","title":"Names in estimates table","type":"Bool","default":true,"description":{"R":"`TRUE` or `FALSE` (default), provide raw names of the contrasts variables\n"}},{"name":"showContrastCode","title":"Contrast Coefficients tables","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), provide contrast coefficients tables\n"}},{"name":"plotHAxis","title":"Horizontal axis","type":"Variable","default":null,"description":{"R":"a string naming the variable placed on the horizontal axis of the plot\n"}},{"name":"plotSepLines","title":"Separate lines","type":"Variable","default":null,"description":{"R":"a string naming the variable represented as separate lines in the plot\n"}},{"name":"plotSepPlots","title":"Separate plots","type":"Variables","default":null,"description":{"R":"a list of string naming the variables defining the levels for multiple plots\n"}},{"name":"plotRaw","title":"Observed scores","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), provide descriptive statistics\n"}},{"name":"plotDvScale","title":"Y-axis observed range","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), scale the plot Y-Axis to the max and the min of the dependent variable observed scores.\n"}},{"name":"plotLinesTypes","title":"Varying line types","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), use different linetypes per levels.\n"}},{"name":"plotError","title":"Error Bar Definition","type":"List","options":[{"name":"none","title":"None"},{"name":"ci","title":"Confidence intervals"},{"name":"se","title":"Standard Error"}],"default":"none","description":{"R":"`'none'`, `'ci'` (default), or `'se'`. Use no error bars, use confidence intervals, or use standard errors on the plots, respectively\n"}},{"name":"ciWidth","title":"Confidence level","type":"Number","min":50,"max":99.9,"default":95,"description":{"R":"a number between 50 and 99.9 (default: 95) specifying the confidence interval width\n"}},{"name":"posthoc","title":"Post Hoc Tests","type":"Terms","default":null,"description":{"R":"a rhs formula with the terms specifying the table to apply the comparisons (of the form `'~x+x:z'`)"}},{"name":"emmeans","title":"Estimated Marginal Means","type":"Terms","default":null,"description":{"R":"a rhs formula with the terms specifying the marginal means to estimate (of the form `'~x+x:z'`)"}},{"name":"simpleVariable","title":"Simple effects variable","type":"Variable","default":null,"description":{"R":"The variable for which the simple effects (slopes) are computed\n"}},{"name":"simpleModerators","title":"Moderators","type":"Variables","default":null,"description":{"R":"the variable that provides the levels at which the simple effects computed\n"}},{"name":"simpleScale","title":"Covariates conditioning","type":"List","options":[{"name":"mean_sd","title":"Mean ±  SD"},{"name":"percent","title":"Percentiles 50 ± offset"}],"default":"mean_sd","description":{"R":"`'mean_sd'` (default), `'custom'` , or `'custom_percent'`. Use to condition the covariates (if any)\n"}},{"name":"cvalue","type":"Number","default":1,"description":{"R":"offset value for conditioning\n"}},{"name":"percvalue","type":"Number","default":25,"min":5,"max":50,"description":{"R":"offset value for conditioning\n"}},{"name":"simpleScaleLabels","title":"Moderators labeling","type":"List","options":[{"name":"labels","title":"Labels"},{"name":"values","title":"Values"},{"name":"values_labels","title":"Values + Labels"},{"name":"uvalues","title":"Unscaled Values"},{"name":"uvalues_labels","title":"Unscaled Values + Labels"}],"default":"labels","description":{"R":"decide the labeling of simple effects in tables and plots.  `labels` indicates that only labels are used, such as `Mean` and  `Mean + 1 SD`. `values` uses the actual values as labels. `values_labels` uses both.\n"}},{"name":"simpleInteractions","title":"Simple Interactions","type":"Bool","default":false,"description":{"R":"should simple Interactions be computed\n"}},{"name":"postHocCorr","title":"Correction","type":"NMXList","options":[{"name":"none","title":"No correction"},{"name":"bonf","title":"Bonferroni"},{"name":"holm","title":"Holm"}],"default":["bonf"],"description":{"R":"one or more of `'none'`,  `'bonf'`, or `'holm'`; provide no,  Bonferroni, and Holm Post Hoc corrections respectively\n"}},{"name":"scaling","title":"Covariates Scaling","type":"Array","items":"(covs)","default":null,"template":{"type":"Group","elements":[{"name":"var","type":"Variable","content":"$key"},{"name":"type","type":"List","options":[{"title":"Centered","name":"centered"},{"title":"z-scores","name":"standardized"},{"title":"Centered clusterwise","name":"clusterbasedcentered"},{"title":"z-scores clusterwise","name":"clusterbasedstandardized"},{"title":"Log","name":"log"},{"title":"None","name":"none"}],"default":"centered"}]},"description":{"R":"a list of lists specifying the covariates scaling, one of `'centered to the mean'`, `'standardized'`, or `'none'`. `'none'` leaves the variable as it is\n"}},{"name":"dep_scale","title":"Scale","type":"List","options":[{"title":"None","name":"none"},{"title":"Centered","name":"centered"},{"title":"z-scores","name":"standardized"},{"title":"Centered clusterwise","name":"clusterbasedcentered"},{"title":"z-scores clusterwise","name":"clusterbasedstandardized"},{"title":"Log","name":"log"}],"default":"none","description":{"R":"Re-scale the dependent variable.\n"}},{"name":"cluster","title":"Cluster variables","type":"Variables","default":null,"suggested":["nominal"],"description":{"R":"a vector of strings naming the clustering variables from `data`"}},{"name":"randomTerms","title":"Random Coefficients","type":"Array","default":[[]],"template":{"type":"Terms"},"description":{"R":"a list of lists specifying the models random effects.          \n"}},{"name":"correlatedEffects","title":"Effects correlation","type":"List","options":[{"name":"corr","title":"Correlated"},{"name":"nocorr","title":"Not correlated"},{"name":"block","title":"Correlated by block"}],"default":"corr","description":{"R":"`'nocorr'`, `'corr'` (default), or `'block'`. When random effects are passed as list of length 1, it decides whether the effects should be correlated,  non correlated. If `'randomTerms'` is a list of  lists of length > 1, the option is automatially set to `'block'`. The option is ignored if the model is passed using `formula`.\n"}},{"name":"reml","title":"REML","type":"Bool","default":true,"description":{"R":"`TRUE` (default) or `FALSE`, should the Restricted ML be used rather than ML\n"}},{"name":"lrtRandomEffects","title":"LRT for Random Effects","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), LRT for the random effects\n"}},{"name":"ciRE","title":"Random Effect C.I. (slow)","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), confidence intervals for the random effects\n"}},{"name":"plotRandomEffects","title":"Random effects","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), add random effects predicted values in the plot\n"}},{"name":"plotOriginalScale","title":"Covariate scale","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), use original scale for covariates.\n"}},{"name":"cimethod","title":"CI Method","type":"List","default":"wald","options":[{"name":"wald","title":"Wald (fast)"},{"name":"profile","title":"Profile (slow)"},{"name":"boot","title":"Bootstrap (very slow)"}]},{"name":"dfmethod","title":"DF Method","type":"List","default":"Satterthwaite","options":[{"name":"Satterthwaite","title":"Satterthwaite"},{"name":"Kenward-Roger","title":"Kenward-Roger"}]},{"name":"qqplot","title":"Q-Q plot of residuals","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), provide a Q-Q plot of residuals\n"}},{"name":"homoTest","title":"Residual Variances Homogeneity tests","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), perform homogeneity tests\n"}},{"name":"normTest","title":"Normality of residuals","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), provide a test for normality of residuals\n"}},{"name":"normPlot","title":"Histogram of residuals","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), provide a histogram of residuals superimposed by a normal distribution\n"}},{"name":"residPlot","title":"Residuals-Predicted scatterplot","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), provide a scatterplot of the residuals against predicted\n"}},{"name":"clusterBoxplot","title":"Residuals by cluster boxplot","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), provide a boxplot of random effects by the first defined clustering variable\n"}},{"name":"clusterResPred","title":"Residuals vs Predcted by cluster","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), residuals vs predicted by the first defined clustering variable\n"}},{"name":"randHist","title":"Histogram of random Coefficients","type":"Bool","default":false,"description":{"R":"`TRUE` or `FALSE` (default), provide histogram of random Coefficients\n"}},{"name":"predicted","title":"Predicted","type":"Output"},{"name":"residuals","title":"Residuals","type":"Output"}];

const view = function() {
    
    

    View.extend({
        jus: "2.0",

        events: [

	],

	update: require('./mixed.events').update

    }).call(this);
}

view.layout = ui.extend({

    label: "Generalized Mixed Model",
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
							height: "small",
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
					style: "list-inline",
					controls: [
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "reml"
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
							name: "showParamsCI",
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox'
						},
						{
							type: DefaultControls.TextBox,
							typeName: 'TextBox',
							name: "paramCIWidth",
							label: "Interval",
							suffix: "%",
							format: FormatDef.number,
							enable: "(showParamsCI)"
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
					higherOrders: true,
					stretchFactor: 1,
					format: FormatDef.term,
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
									name: "modelTerms",
									valueFilter: "unique",
									isTarget: true,
									itemDropBehaviour: "emptyspace",
									events: [
										{ execute: require('./mixed.events').onChange_modelTerms }
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
					margin: "large",
					controls: [
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "fixedIntercept"
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
					persistentItems: false,
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
							controls: [
								{
									type: DefaultControls.ListBox,
									typeName: 'ListBox',
									name: "randomTerms",
									height: "large",
									addButton: "Add block",
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
												name: "randblockList",
												isTarget: true,
												valueFilter: "unique",
												height: "auto",
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
											name: "correlatedEffects_corr",
											optionName: "correlatedEffects",
											optionPart: "corr",
											events: [
												{ execute: require('./mixed.events').onEvent_corr }
											]
										},
										{
											type: DefaultControls.RadioButton,
											typeName: 'RadioButton',
											name: "correlatedEffects_nocorr",
											optionName: "correlatedEffects",
											optionPart: "nocorr"
										},
										{
											type: DefaultControls.RadioButton,
											typeName: 'RadioButton',
											name: "correlatedEffects_block",
											optionName: "correlatedEffects",
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
											name: "lrtRandomEffects"
										},
										{
											name: "ciRE",
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox'
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
					name: "showRealNames"
				},
				{
					type: DefaultControls.CheckBox,
					typeName: 'CheckBox',
					name: "showContrastCode"
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
					name: "scaling",
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
											name: "simpleScale_mean_sd",
											optionName: "simpleScale",
											optionPart: "mean_sd",
											controls: [
												{
													type: DefaultControls.TextBox,
													typeName: 'TextBox',
													name: "cvalue",
													format: FormatDef.number
												}
											]
										},
										{
											name: "simpleScale_percent",
											type: DefaultControls.RadioButton,
											typeName: 'RadioButton',
											optionName: "simpleScale",
											optionPart: "percent",
											controls: [
												{
													type: DefaultControls.TextBox,
													typeName: 'TextBox',
													name: "percvalue",
													label: null,
													suffix: "%",
													format: FormatDef.number,
													enable: "(simpleScale_percent)"
												}
											]
										}
									]
								}
							]
						},
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "Covariates labeling",
							controls: [
								{
									type: DefaultControls.LayoutBox,
									typeName: 'LayoutBox',
									margin: "large",
									controls: [
										{
											type: DefaultControls.RadioButton,
											typeName: 'RadioButton',
											name: "simpleScaleLabels_labels",
											optionName: "simpleScaleLabels",
											optionPart: "labels"
										},
										{
											type: DefaultControls.RadioButton,
											typeName: 'RadioButton',
											name: "simpleScaleLabels_numbers",
											optionName: "simpleScaleLabels",
											optionPart: "values"
										},
										{
											type: DefaultControls.RadioButton,
											typeName: 'RadioButton',
											name: "simpleScaleLabels_numbers_labels",
											optionName: "simpleScaleLabels",
											optionPart: "values_labels"
										},
										{
											name: "simpleScaleLabels_uvalues",
											type: DefaultControls.RadioButton,
											typeName: 'RadioButton',
											optionName: "simpleScaleLabels",
											optionPart: "uvalues"
										},
										{
											name: "simpleScaleLabels_uvalues_labels",
											type: DefaultControls.RadioButton,
											typeName: 'RadioButton',
											optionName: "simpleScaleLabels",
											optionPart: "uvalues_labels"
										}
									]
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
					name: "postHocSupplier",
					persistentItems: false,
					stretchFactor: 1,
					format: FormatDef.term,
					events: [
						{ execute: require('./gamlj.events').onChange_postHocSupplier },
						{ onEvent: 'update', execute: require('./gamlj.events').onUpdate_postHocSupplier }
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
					controls: [
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "Correction",
							controls: [
								{
									name: "postHocCorr_none",
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									optionPart: "none",
									optionName: "postHocCorr"
								},
								{
									name: "postHocCorr_bonf",
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									optionPart: "bonf",
									optionName: "postHocCorr"
								},
								{
									name: "postHocCorr_holm",
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									optionPart: "holm",
									optionName: "postHocCorr"
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
						{ execute: require('./gamlj.events').onChange_plotsSupplier },
						{ onEvent: 'update', execute: require('./gamlj.events').onUpdate_plotsSupplier }
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
									optionPart: "ci",
									controls: [
										{
											type: DefaultControls.TextBox,
											typeName: 'TextBox',
											name: "ciWidth",
											label: "Interval",
											suffix: "%",
											format: FormatDef.number,
											enable: "(errBarDef_ci)"
										}
									]
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
								},
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "plotRandomEffects"
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
									name: "simpleVariable",
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
									name: "simpleModerators",
									isTarget: true
								}
							]
						}
					]
				},
				{
					type: DefaultControls.CheckBox,
					typeName: 'CheckBox',
					name: "simpleInteractions",
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
					label: "Residuals",
					controls: [
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "normTest",
							label: "Test normality of residuals"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "homoTest"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "qqplot",
							label: "Q-Q plot of residuals"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "normPlot",
							label: "Residuals histogram"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "residPlot",
							label: "Residuals-Predicted plot"
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
							name: "clusterBoxplot",
							label: "Residuals boxplot"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "clusterResPred",
							label: "Residuals-Predcted by cluster"
						},
						{
							type: DefaultControls.CheckBox,
							typeName: 'CheckBox',
							name: "randHist",
							label: "Random coefficients histogram"
						}
					]
				}
			]
		},
		{
			type: DefaultControls.CollapseBox,
			typeName: 'CollapseBox',
			label: "Options",
			style: "inline",
			collapsed: true,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.Label,
					typeName: 'Label',
					label: "CI Method",
					controls: [
						{
							type: DefaultControls.RadioButton,
							typeName: 'RadioButton',
							name: "cimethod_wald",
							optionName: "cimethod",
							optionPart: "wald"
						},
						{
							type: DefaultControls.RadioButton,
							typeName: 'RadioButton',
							name: "cimethod_profile",
							optionName: "cimethod",
							optionPart: "profile"
						},
						{
							type: DefaultControls.RadioButton,
							typeName: 'RadioButton',
							name: "cimethod_boot",
							optionName: "cimethod",
							optionPart: "boot"
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
							name: "dfmethod_Satterthwaite",
							optionName: "dfmethod",
							optionPart: "Satterthwaite"
						},
						{
							type: DefaultControls.RadioButton,
							typeName: 'RadioButton',
							name: "dfmethod_Kenward-Roger",
							optionName: "dfmethod",
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
});

module.exports = { view : view, options: options };
