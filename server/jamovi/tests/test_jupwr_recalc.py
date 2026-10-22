"""jUPWR: lazy IF and the single pass of Column.recalc (task 24, point 13)"""

import logging
import math

from jamovi.server.datasetmodel import DataSetModel
from jamovi.server.dataset import ColumnType, DataType, MeasureType


def _model_with_x(model, values):
    x = model.append_column('x')
    x.change(data_type=DataType.DECIMAL, measure_type=MeasureType.CONTINUOUS)
    model.set_row_count(len(values))
    for i, v in enumerate(values):
        x.set_value(i, v)
    return x


def _computed(model, name, formula):
    column = model.append_column(name)
    column.column_type = ColumnType.COMPUTED
    column.formula = formula
    column.parse_formula()
    return column


def _missing(value):
    return value == '' or value == -2147483648 or (isinstance(value, float) and math.isnan(value))


def test_if_computes_only_the_chosen_branch(dataset_model: DataSetModel):
    # LN(-1) raises; the eager IF made the whole row missing
    m = dataset_model
    _model_with_x(m, [ -1.0, math.e, float('nan') ])
    c = _computed(m, 'c', 'IF(x > 0, LN(x), 0)')
    m._recalc_all()
    assert c.formula_message == ''
    assert c[0] == 0
    assert c[1] == 1
    assert _missing(c[2])


def test_if_defaults(dataset_model: DataSetModel):
    m = dataset_model
    _model_with_x(m, [ 1.0, 0.0 ])
    one = _computed(m, 'one', 'IF(x)')
    two = _computed(m, 'two', 'IF(x == 1, 5)')
    m._recalc_all()
    assert [ one[0], two[0] ] == [ 1, 5 ]
    assert _missing(one[1]) and _missing(two[1])


def test_rules_cells_and_levels_in_one_pass(dataset_model: DataSetModel):
    m = dataset_model
    xs = [ 5.0, 55.0, 95.0, float('nan'), 35.0, 75.0 ]
    _model_with_x(m, xs)
    r = _computed(m, 'r', '_RULES_ORD(IF(x < 30, "nisko", IF(x < 60, "średnio", "wysoko")))')
    m._recalc_all()
    assert [ level[1] for level in r.levels ] == [ 'nisko', 'średnio', 'wysoko' ]
    cells = [ r[i] for i in range(len(xs)) ]
    assert cells[:3] == [ 'nisko', 'średnio', 'wysoko' ]
    assert _missing(cells[3])
    assert cells[4:] == [ 'średnio', 'wysoko' ]

    # a cell edit, recalculated the way the controller does
    m['x'].set_value(0, 99.0)
    r.set_needs_recalc()
    r.recalc()
    assert r[0] == 'wysoko'
    assert [ level[1] for level in r.levels ] == [ 'średnio', 'wysoko' ]


def test_rows_in_error_stay_missing(dataset_model: DataSetModel):
    # the condition itself raises for x <= 0: those rows are missing and do
    # not count towards the levels (the error is logged; the test model has
    # a null log without exception())
    m = dataset_model
    m._log = logging.getLogger('test_jupwr_recalc')
    _model_with_x(m, [ -1.0, 10.0, 1.0 ])
    r = _computed(m, 'r', '_RULES_NOM(IF(LN(x) > 1, "duże", "małe"))')
    m._recalc_all()
    assert _missing(r[0])
    assert [ r[1], r[2] ] == [ 'duże', 'małe' ]
    assert sorted(level[1] for level in r.levels) == [ 'duże', 'małe' ]
