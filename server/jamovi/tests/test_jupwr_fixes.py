"""jUPWR: regression tests for the 2026-10 audit fixes (1: import from
.omv, 4: rule levels with categories from a column, is_rules and quoted
text)"""

from os import path

import pytest

from jamovi.server import jamovi_pb2 as jcoms
from jamovi.server.instance import Instance
from jamovi.server.session import Session
from jamovi.server.datasetmodel import DataSetModel
from jamovi.server.dataset import ColumnType, DataType, MeasureType
from jamovi.server.transform import Transform


class FakeComs:
    def __init__(self):
        self.sent = [ ]
        self.errors = [ ]

    def send(self, message=None, instance_id=None, response_to=None,
             complete=True, progress=(0, 0), status=None):
        self.sent.append(message)

    def send_error(self, message=None, cause=None, instance_id=None, response_to=None):
        self.errors.append((message, cause))

    def add_close_listener(self, listener):
        pass

    def remove_close_listener(self, listener):
        pass


async def _open(instance, file_path='', **kwargs):
    stream = instance.open(file_path, **kwargs)
    async for _ in stream:
        pass
    return await stream


async def _save(instance, file_path):
    stream = instance.save({ 'path': file_path, 'overwrite': True })
    async for _ in stream:
        pass
    return await stream


async def _set_text(instance, values):
    request = jcoms.DataSetRR()
    request.op = jcoms.GetSet.Value('SET')
    request.incData = True
    block = request.data.add()
    block.rowStart = 0
    block.columnStart = 1
    block.rowCount = len(values)
    block.columnCount = 1
    for value in values:
        block.values.add().s = value
    await instance.on_request(request)


@pytest.mark.asyncio
async def test_import_from_omv(session: Session, temp_dir: str):
    source = await session.create()
    await _open(source)
    source.set_coms(FakeComs())
    await _set_text(source, ('x', 'y', 'x'))
    omv_path = path.join(temp_dir, 'import-source.omv')
    await _save(source, omv_path)
    source.close()

    target = await session.create()
    await _open(target)
    coms = FakeComs()
    target.set_coms(coms)
    await _set_text(target, ('p', 'q'))

    request = jcoms.OpenRequest()
    request.op = jcoms.OpenRequest.Op.Value('IMPORT_REPLACE')
    request.filePath = omv_path
    await target.on_request(request)

    assert coms.errors == [ ]
    dataset = target.project.get_dataset()
    assert dataset.row_count == 3
    values = [ [ dataset[c][r] for r in range(3) ] for c in range(dataset.column_count) ]
    assert [ 'x', 'y', 'x' ] in values
    target.close()


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


@pytest.mark.parametrize('formula, rules', [
    ('IF(a == 1, "$source", "x")', True),       # the text "$source" is just text
    ("IF(a == 1, '$source', 'x')", True),
    ('IF(a == 1, \'say "$source"\', "x")', True),
    ('IF(a == 1, "x", $source)', False),        # a real reference to the source
    ('$source', False),                         # an empty upstream transform
])
def test_is_rules_ignores_source_in_quoted_text(formula, rules):
    transform = Transform(None)
    transform.formula = [ formula ]
    assert transform.is_rules is rules


def test_rule_levels_follow_rule_order_with_numbers(dataset_model: DataSetModel):
    # an ordinal scale mixing numbers and text keeps the order of the rules
    # (it used to list the texts first, then the numbers)
    r = _rules_column(dataset_model,
        '_RULES_ORD(IF(ROW() == 1, 3, IF(ROW() == 2, "średnio", 1)))', [ 1, 1, 1 ])
    assert [ level[1] for level in r.levels ] == [ '3', 'średnio', '1' ]
    assert [ r[i] for i in range(3) ] == [ '3', 'średnio', '1' ]


def test_rule_levels_with_filter_and_column_function(dataset_model: DataSetModel):
    # Z() is computed on the filtered rows for the cells; the levels must be
    # too, or the cells hold values that are not among the levels
    m = dataset_model
    x = m.append_column('x')
    x.change(data_type=DataType.DECIMAL, measure_type=MeasureType.CONTINUOUS)
    m.set_row_count(4)
    for i, v in enumerate([ 1.0, 2.0, 3.0, 1000.0 ]):
        x.set_value(i, v)
    f = m.insert_column(0)
    f.column_type = ColumnType.FILTER
    f.formula = 'x < 500'
    f.parse_formula()
    m.refresh_filter_state()
    r = m.append_column('r')
    r.column_type = ColumnType.COMPUTED
    # with the filter Z = -1, 0, 1; without it 1, 2, 3 are all "pod"
    r.formula = '_RULES_NOM(IF(Z(x) > 0.3, "nad", IF(Z(x) < -0.3, "pod", "środek")))'
    r.parse_formula()
    m._recalc_all()
    cells = [ r[i] for i in range(3) ]
    labels = [ level[1] for level in r.levels ]
    assert cells == [ 'pod', 'środek', 'nad' ]
    assert labels == [ 'nad', 'pod', 'środek' ]


@pytest.mark.parametrize('formula, value', [
    ('"a\\b"', 'a\\b'),                 # a backslash is just a character
    ("'say \"hi\"'", 'say "hi"'),       # double quotes inside single quotes
    ('"O\'Brien"', "O'Brien"),
])
def test_quoted_text_is_literal(formula, value):
    from jamovi.server.compute.parser import Parser
    node = Parser.parse(formula)
    assert node.value == value
