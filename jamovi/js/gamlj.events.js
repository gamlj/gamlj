var rtermFormat = require('./rtermFormat');
var fun=require('./functions');

const events = {
    update: function(ui) {
        this.setCustomVariable("Intercept", "none", "");
        fun.calcModelTerms(ui, this);
        fun.filterModelTerms(ui, this);
        fun.updatePostHocSupplier(ui, this);
        fun.updateSimpleSupplier(ui, this);
        fun.updatePlotsSupplier(ui, this);

        if (typeof ui.randomSupplier !== 'undefined' ) {
              fun.fixRandomEffects(ui,this);
        }


        if (typeof ui.comparison !== 'undefined' ) {
              fun.fix_comparison(ui,this);
        }
        
        if (typeof ui.propodds_test !== 'undefined' ) {
          
            if (ui.model_type.getValue()==="ordinal") {
              ui.propodds_test.$el.show();
            } else {
              ui.propodds_test.$el.hide();
            }
        }


    },

    onChange_factors: function(ui) {
        fun.calcModelTerms(ui, this);
        fun.updateRandomSupplier(ui,this);
    },

    onChange_covariates: function(ui) {
        fun.calcModelTerms(ui, this);
        fun.updateRandomSupplier(ui,this);
    },

    onChange_model_terms: function(ui) {
        fun.filterModelTerms(ui, this);
        fun.updatePostHocSupplier(ui, this);
        fun.updateSimpleSupplier(ui, this);
        fun.updateEmmeansSupplier(ui, this);
        fun.updatePlotsSupplier(ui, this);
        fun.updateRandomSupplier(ui,this);

    },
    onChange_nested_terms: function(ui) {
    },
  
    onChange_nested_add: function(ui) {

      let tvalues=this.cloneArray(ui.model_terms.value(),[]);
      let nvalues=this.cloneArray(ui.nested_terms.value(),[]);
      var filtered = nvalues.filter(function(item) {
         return tvalues.some(function(jtem) {
           return FormatDef.term.isEqual(jtem, item);
         });
      });
      if (filtered.length !== nvalues.length)
                  ui.nested_terms.setValue(filtered);
    },
    onEvent_comparison: function(ui) {
         
         fun.fix_comparison(ui, this);

    },
    onChange_plotsSupplier: function(ui) {
        let values = this.itemsToValues(ui.plotsSupplier.value());
        this.checkValue(ui.plot_x, false, values, FormatDef.variable);
        this.checkValue(ui.plot_z, false, values, FormatDef.variable);
        this.checkValue(ui.plot_by, true, values, FormatDef.variable);
    },
    onUpdate_plotsSupplier: function(ui) {
        fun.updatePlotsSupplier(ui, this);
    },
    
    onChange_simpleSupplier: function(ui) {
        let values = this.itemsToValues(ui.simpleSupplier.value());
        this.checkValue(ui.simple_effects, false, values, FormatDef.variable);
        this.checkValue(ui.simple_moderators, true, values, FormatDef.variable);
    },

    onUpdate_simpleSupplier: function(ui) {
        fun.updateSimpleSupplier(ui, this);
    },

     onChange_model: function(ui) {

        if (typeof ui.es_RR !== 'undefined' ) {
              ui.es_RR.setValue(false);
        }
        if (typeof ui.plot_scale !== 'undefined' ) {
              ui.plot_scale.setValue('response');
        }

        if (ui.model_type.getValue()==="custom" ||  ui.model_type.getValue()==="linear")        {
               ui.es_expb.setValue(false);
               ui.estimates_ci.setValue(true);
               ui.expb_ci.setValue(false);
        } else  {
               ui.es_expb.setValue(true);
               ui.estimates_ci.setValue(false);
               ui.expb_ci.setValue(true);
        }

        if (typeof ui.propodds_test !== 'undefined') {
              if (ui.model_type.getValue()==="ordinal") {
                  ui.propodds_test.$el.show();
              } else {
                  ui.propodds_test.$el.hide();
              }
        }

        if (  ui.model_type.getValue() === 'ordinal') {
              ui.ci_method_wald.setValue(true);
              ui.ci_method_quantile.setEnabled(false);
              ui.ci_method_bcai.setEnabled(false);
        }
  
        ui.dep.setValue(null);
      },

    onChange_model_remove: function(ui) {
      let values=this.cloneArray(ui.model_terms.value(),[]);
      this.checkValue(ui.nested_terms, true, values, FormatDef.term);
      
    },
    onChange_posthocSupplier: function(ui) {
        let values = this.itemsToValues(ui.posthocSupplier.value());
        this.checkValue(ui.posthoc, true, values, FormatDef.term);
    },

    onUpdate_posthocSupplier: function(ui) {
        fun.updatePostHocSupplier(ui, this);
    },

    onChange_emmeansSupplier: function(ui) {
        let values = this.itemsToValues(ui.emmeansSupplier.value());
        this.checkValue(ui.emmeans, true, values, FormatDef.term);

    },

    onUpdate_emmeansSupplier: function(ui) {
        fun.updateEmmeansSupplier(ui, this);
    },

    
    onUpdate_modelSupplier: function(ui) {
            let factorsList = this.cloneArray(ui.factors.value(), []);
            let covariatesList = this.cloneArray(ui.covs.value(), []);
            var variablesList = factorsList.concat(covariatesList);
            ui.modelSupplier.setValue(this.valuesToItems(variablesList, FormatDef.variable));
    },
    onChange_cluster: function(ui) {
        fun.updateRandomSupplier(ui,this);
    },

    onChange_randomSupplier: function(ui){
      
        let supplierList = this.itemsToValues(ui.randomSupplier.value());
        var changes = this.findChanges("randomSupplier",supplierList,rtermFormat);
        if (changes.removed.length>0) {
          var re = this.cloneArray(ui.re.value(),[]);
          var  light = removeFromMultiList(changes.removed,re,this,1);
          ui.re.setValue(light);
        }
        return;
    },
    onUpdate_randomSupplier: function(ui) {
        fun.updateRandomSupplier(ui,this);

    },
    onEvent_re_list: function(ui) {
      fun.updateRandomSupplier(ui,this);
    },
    onEvent_corr: function(ui, data) {
          console.log("Correlation structure changed");
          fun.fixRandomEffects(ui,this);
    },    

    onChange_nested_re_add: function(ui) {
//          console.log("I didn't do anything");
    },
    onEvent_addRandomTerm: function(ui) {
//        console.log("addRandomTerm does nothing");
    },

   onEvent_nothing: function(ui, data) {
//          console.log("I didn't do anything");
    }    

};


module.exports = events;



// local functions 

var removeFromMultiList = function(quantum, cosmos, context, strict = 1) {

    var cosmos = context.cloneArray(cosmos);
    var dimq = dim(quantum);
        for (var j = 0; j < cosmos.length; j++) 
           cosmos[j]=removeFromList(quantum,cosmos[j],context, strict);
    return(cosmos);
};


