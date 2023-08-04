    module.exports = {



        view_updated: function(ui, event) {
            this.setPanels(ui, event);
        },

        view_creating: function(ui, event) {
            this.setPanels(ui, event);
        },

        view_loaded: function(ui, event) {
            this.setPanels(ui, event);
        },

        conf_level_changed: function(ui, event) {
            this.setPanels(ui, event);
        },

        switch_changed: function(ui, event) {
            ui.effect_size.setValue("mean");
        },


        setPanels: function(ui, event) {
             ui.alpha.setValue((1 - Number(ui.conf_level.value())/100).toPrecision(1));
             if (ui.switch.value() == 'from_summary') {
                ui.effect_size.setValue("mean");
             }
        }


    };
