    module.exports = {

        // event handlers and functions are defined here

        // this is an example of an event handler
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
            ui.alpha.setValue((1 - Number(ui.conf_level.value())/100).toPrecision(1));
        },

        raw_button_changed: function(ui, event) {
            this.setPanels(ui, event);
        },

        effect_size_changed: function(ui, event) {
            ui.show_calculations.setValue(false);
        },

        case_label_changed: function(ui, event) {
            ui.not_case_label.setValue("Not " + ui.case_label.value());
        },


        // this is an example of an auxiliary function
        setPanels: function(ui, event) {
            // check if summary or raw data option selected, and then set panel and enabled states appropriately
            if (ui.summary_button.value()){
              // summary data option - expand that panel and collapse the raw panel
              // also set all controls in summary panel to be enabled
              // would be nice to be able to disable the controls in the raw data panel and/or expand option for that panel
              ui.spanel.expand();
              ui.rpanel.collapse();
            } else {
              // raw data selected, so expand that panel, collapse summary data panel and disable its controls
              // would be nice to disable collapse/expand options on the panels, but doesn't seem possible at the moment
              ui.spanel.collapse();
              ui.rpanel.expand();
            }
            ui.alpha.setValue((1 - Number(ui.conf_level.value())/100).toPrecision(1));

        }


    };
