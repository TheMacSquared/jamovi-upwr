"""jUPWR: regression tests for the 2026-10 audit fixes"""

from uuid import uuid4

import pytest_asyncio

from jamovi.server.instancemodel import InstanceModel
from jamovi.server.session import Session
from jamovi.server.dataset import ColumnType, DataType, MeasureType


# conftest's instance_model builds its Session outside the event loop,
# which this Python refuses; build one inside it (as 1.1.x does)
@pytest_asyncio.fixture
async def rules_model(temp_dir, empty_dataset) -> InstanceModel:
    session = Session(temp_dir, str(uuid4()))
    im = InstanceModel(await session.create())
    im._dataset = empty_dataset
    return im


def _rules_column(model, formula, b_codes):
    # b: a nominal integer column with labels (e.g. from SPSS), whose values
    # reach the compute nodes as (code, label) tuples
    m = model
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


def test_rule_levels_category_and_decimal(rules_model: InstanceModel):
    # used to raise TypeError (tuple < float) while sorting the levels
    r = _rules_column(rules_model, '_RULES_NOM(IF(ROW() <= 2, b, 3.5))', [ 2, 1, 1 ])
    assert [ level[1] for level in r.levels ] == [ 'kot', 'pies', '3.5' ]
    assert [ r[i] for i in range(3) ] == [ 'pies', 'kot', '3.5' ]


def test_rule_levels_category_and_text(rules_model: InstanceModel):
    # rule order: the column's categories (in their own order), then "inny"
    r = _rules_column(rules_model, '_RULES_NOM(IF(ROW() <= 2, b, "inny"))', [ 2, 1, 1 ])
    assert r.formula_message == ''
    assert [ level[1] for level in r.levels ] == [ 'kot', 'pies', 'inny' ]
    assert [ r[i] for i in range(3) ] == [ 'pies', 'kot', 'inny' ]
