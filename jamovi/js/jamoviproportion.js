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

        case_label_changed: function(ui, event) {
            ui.not_case_label.setValue("Not " + ui.case_label.value());
        },

        setPanels: function(ui, event) {

            ui.alpha.setValue((1 - Number(ui.conf_level.value())/100).toPrecision(1));

        }


    };
