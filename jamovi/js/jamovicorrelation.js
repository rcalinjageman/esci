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

        do_regression_changed: function(ui, event) {
          if (ui.do_regression.value()) {

          } else {
            ui.predict_from_x.setValue("");
            ui.show_line.setValue(false);
            ui.show_line_CI.setValue(false);
            ui.show_residuals.setValue(false);
            ui.show_PI.setValue(false);
            ui.show_mean_lines.setValue(false);
            ui.plot_as_z.setValue(false);
          }
        },


        // this is an example of an auxiliary function
        setPanels: function(ui, event) {
            ui.alpha.setValue((1 - Number(ui.conf_level.value())/100).toPrecision(1));
        }

    };
