'use strict';

// The "event" LevelSelector lists the categories of the FIRST measurement:
// a Level option points at a single variable, while there are several here
// (they share the same two categories anyway, as the tests require).

const syncEvent = function(ui) {
    let vars = ui.vars.value();
    let first = (Array.isArray(vars) && vars.length > 0) ? vars[0] : null;
    ui.event.setPropertyValue('variable', first);
};

module.exports = {
    view_updated: syncEvent,
    vars_changed: syncEvent
};
