'use strict';

// Reference levels: the ListBox row for each factor needs to know which
// variable its LevelSelector belongs to (pattern from jmv linreg.events.js).

const syncLevels = function(ui) {
    let rows = ui.refLevels.value();
    if (!rows) return;
    ui.refLevels.applyToItems(0, (item, index, column) => {
        if (column === 1 && rows[index])
            item.setPropertyValue('variable', rows[index].var);
    });
};

module.exports = {
    view_updated: syncLevels,
    refLevels_changed: syncLevels
};
