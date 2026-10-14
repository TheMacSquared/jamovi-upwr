"""jUPWR: regression tests for the 2026-10 audit fixes"""

from jamovi.server.datasetmodel import DataSetModel
from jamovi.server.dataset import ColumnType, DataType, MeasureType


def _rules_column(dataset_model, formula, b_codes):
    # b: a nominal integer column with labels (e.g. from SPSS), whose values
    # reach the compute nodes as (code, label) tuples
    m = dataset_model
    b = m.append_column('b')
    b.change(data_type=DataType.INTEGER, measure_type=MeasureType.NOMINAL)
    b.append_level(1, 'kot', '1')
    b.append_level(2, 'pies', '2')
    m.set_row_count(len(b_codes))
    for i, v in enumerate(b_codes):
        b.set_value(i, v)
    r = m.append_column('r')
    r.column_type = ColumnType.COMPUTED
    r.formula = formula
    r.parse_formula()
    m._recalc_all()
    return r


def test_rule_levels_category_and_decimal(dataset_model: DataSetModel):
    # used to raise TypeError (tuple < float) while sorting the levels
    r = _rules_column(dataset_model, '_RULES_NOM(IF(ROW() <= 2, b, 3.5))', [ 2, 1, 1 ])
    assert [ level[1] for level in r.levels ] == [ 'kot', 'pies', '3.5' ]
    assert [ r[i] for i in range(3) ] == [ 'pies', 'kot', '3.5' ]


def test_rule_levels_category_and_text(dataset_model: DataSetModel):
    # rule order: the column's categories (in their own order), then "inny"
    r = _rules_column(dataset_model, '_RULES_NOM(IF(ROW() <= 2, b, "inny"))', [ 2, 1, 1 ])
    assert r.formula_message == ''
    assert [ level[1] for level in r.levels ] == [ 'kot', 'pies', 'inny' ]
    assert [ r[i] for i in range(3) ] == [ 'pies', 'kot', 'inny' ]
