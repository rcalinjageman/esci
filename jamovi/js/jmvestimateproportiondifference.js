    module.exports = {

        // event handlers and functions are defined here

        // this is an example of an event handler
        view_loaded: function(ui, event) {
            // do something
            this.setPanels(ui, event);
        },

        // this is another example of an event handler
        switchr_changed: function(ui, event) {
            this.setPanels(ui, event);
        },

        // this is an example of an auxiliary function
        setPanels: function(ui, event) {
            // check if summary or raw data option selected, and then set panel and enabled states appropriately
            if (ui.switchs.value()){
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
            
            ui.cases1.setEnabled(ui.switchs.value());
            ui.n1.setEnabled(ui.switchs.value());
            ui.cases2.setEnabled(ui.switchs.value());
            ui.n2.setEnabled(ui.switchs.value());
            ui.caselabel1.setEnabled(ui.switchs.value());
            ui.caselabel2.setEnabled(ui.switchs.value());
            ui.grouplabel1.setEnabled(ui.switchs.value());
            ui.grouplabel2.setEnabled(ui.switchs.value());
        }


    };