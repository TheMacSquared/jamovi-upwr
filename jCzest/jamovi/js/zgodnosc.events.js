
// Synchronizuje listę proporcji oczekiwanych z poziomami wybranej zmiennej.
// Bez tego użytkownik wpisywałby liczby, nie widząc, do której kategorii trafiają
// — a kolejność poziomów jest alfabetyczna, więc pomyłka byłaby cicha.

const events = {
    update: function(ui) {
        this._updating = 0;
        updateRatios(ui, this);
    },

    onChange_var: function(ui) {
        updateRatios(ui, this);
    },

    onChange_ratio: function(ui) {
        updateRatios(ui, this);
    },

    onRemoteDataChanged: function(ui, data) {
        if (data.dataType !== 'columns' || data.levelChanged === false)
            return;
        updateRatios(ui, this);
    }
};

const updateRatios = function(ui, context) {

    if (context._updating > 0)
        return;
    context._updating += 1;

    const columnName = ui.var.value();
    const oldRatios = context.clone(ui.ratio.value(), []);

    const promise = context.requestData('column', {
        columnName: columnName,
        properties: ['levels']
    });

    promise.then(rData => {
        let data = [];
        let removed = 0;

        if (rData.columnFound) {
            const levels = rData.levels;
            // poziomy odfiltrowane albo oznaczone jako braki nie wchodzą do testu
            const kept = levels.filter(l => !(l.treatAsMissing || l.filtered));
            removed = levels.length - kept.length;

            // suma wag potrzebna, żeby pokazać udział obok każdej wagi
            let total = 0;
            for (let i = 0; i < kept.length; i++) {
                const prev = (i < oldRatios.length) ? oldRatios[i].ratio : 1;
                total += (prev === undefined || prev === null) ? 1 : prev;
            }
            if (total <= 0)
                total = kept.length;

            for (let i = 0; i < kept.length; i++) {
                let ratio = 1;
                if (i < oldRatios.length && oldRatios[i].ratio !== undefined
                        && oldRatios[i].ratio !== null)
                    ratio = oldRatios[i].ratio;

                const prop = (Math.round((ratio / total) * 1000) / 1000).toFixed(3);
                data.push({ level: kept[i].label, ratio: ratio, proportion: prop });
            }
        }

        ui.ratio.setValue(data);

        let msg = null;
        if (removed === 1)
            msg = 'Filtry wykluczyły jeden poziom.';
        else if (removed > 1)
            msg = `Filtry wykluczyły ${removed} poziomy.`;
        ui.ratio.setPropertyValue('infoText', msg);

        context._updating -= 1;
    });
};

module.exports = events;
