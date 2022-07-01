
const events = {
  
     onChange_model: function(ui) {

        if (typeof ui.es_RR !== 'undefined' ) {
              ui.es_RR.setValue(false);
        }
        
        if (ui.model_type.getValue()==="custom" ||  ui.model_type.getValue()==="linear"){
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
              ui.boot_r.setEnabled(false);
        } else {
          
              ui.ci_method_quantile.setEnabled(true);
              ui.ci_method_bcai.setEnabled(true);
              ui.boot_r.setEnabled(true);
          
          
        }
  
        ui.dep.setValue(null);
      }


};


module.exports = events;

