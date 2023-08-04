    module.exports = {


        view_updated: function(ui, event) {
            this.setPanels(ui, event);
        },

        view_creating: function(ui, event) {
            this.setPanels(ui, event);
        },

        view_loaded: function(ui, event) {
            // do something
            this.setPanels(ui, event);
        },

        conf_level_changed: function(ui, event) {
          this.setPanels(ui, event);
        },


        effect_size_changed: function(ui, event) {
          if (ui.effect_size.getValue() == "median_difference") {
                ui.error_layout.setValue("none");
            }
        },

        switch_changed: function(ui, event) {
            this.setPanels(ui, event);
        },

        setPanels: function(ui, event) {

            if (ui.switch.value() == 'from_summary') {
              ui.effect_size.setValue("mean_difference");
            }
            ui.alpha.setValue((1 - Number(ui.conf_level.value())/100).toPrecision(1));


        }


    };
