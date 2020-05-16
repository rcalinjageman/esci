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
              ui.m1.setEnabled(true);
              ui.m2.setEnabled(true);
              ui.s1.setEnabled(true);
              ui.s2.setEnabled(true);
              ui.n1.setEnabled(true);
              ui.n2.setEnabled(true);
              ui.g1lab.setValue("Reference Group");
              ui.g2lab.setValue("Comparison Group");
            } else {
              // raw data selected, so expand that panel, collapse summary data panel and disable its controls
              // would be nice to disable collapse/expand options on the panels, but doesn't seem possible at the moment
              ui.spanel.collapse();
              ui.rpanel.expand();
              ui.m1.setEnabled(false);
              ui.m2.setEnabled(false);
              ui.s1.setEnabled(false);
              ui.s2.setEnabled(false);
              ui.n1.setEnabled(false);
              ui.n2.setEnabled(false);
              ui.g1lab.setValue("auto");
              ui.g2lab.setValue("auto");
            }
        }


    };