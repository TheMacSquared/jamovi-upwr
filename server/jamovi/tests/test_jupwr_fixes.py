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


# upstream jamovi 28.13 (dcfbcbd6): the analysis state is not restored from
# .omv files -- R's serialization runs whatever code a crafted file holds.
# The tests are those of upstream, without its svg fixtures (28.13 only).

import asyncio

import pytest

from jamovi.server import jamovi_pb2 as jcoms
from jamovi.server.analyses import Analysis, Analyses
from jamovi.server.options import Options


class _FakeDataset:
    def __init__(self, instance_path):
        self.instance_path = instance_path


class _FakeModules:
    def add_listener(self, listener):
        pass


class _FakeParent:
    def _notify_options_changed(self, analysis):
        pass

    def _notify_results_changed(self, analysis):
        pass

    def index_of(self, analysis):
        return 0


def test_state_is_dropped_from_files(temp_dir):
    analysis_pb = jcoms.AnalysisResponse()
    analysis_pb.name = 'empty'
    analysis_pb.ns = 'jmv'
    analysis_pb.results.state = b'top'

    image_pb = analysis_pb.results.group.elements.add()
    image_pb.name = 'plot'
    image_pb.state = b'plot'
    image_pb.image.path = '01 empty/resources/plot.png'

    array_pb = analysis_pb.results.group.elements.add()
    array_pb.name = 'array'
    array_pb.array.header.state = b'header'
    item_pb = array_pb.array.elements.add()
    item_pb.group.elements.add().state = b'nested'

    analyses = Analyses(_FakeDataset(temp_dir), _FakeModules())
    analysis = analyses.create_from_serial(analysis_pb.SerializeToString())

    results_pb = analysis.results.results
    assert results_pb.state == b''
    assert results_pb.group.elements[0].state == b''
    assert results_pb.group.elements[1].array.header.state == b''
    assert results_pb.group.elements[1].array.elements[0].group.elements[0].state == b''
    # the image still displays from the file
    assert results_pb.group.elements[0].image.path == '01 empty/resources/plot.png'
    # and the analysis runs from scratch before anything needs the state
    assert analysis.state_dropped is True


@pytest.mark.asyncio
async def test_export_runs_the_analysis_first(temp_dir):
    analysis = Analysis(
        _FakeDataset(temp_dir), 2, 'descriptives', 'jmv', Options.create({}), _FakeParent(), enabled=True)
    analysis.results = jcoms.AnalysisResponse()
    analysis.complete = True
    analysis.set_status(Analysis.Status.COMPLETE)
    analysis.state_dropped = True

    save = asyncio.create_task(analysis.save('/tmp/plot.png', 'plot'))
    await asyncio.sleep(0)

    assert analysis.complete is False
    assert analysis.status is Analysis.Status.NONE
    assert analysis.clear_state is True
    assert analysis.needs_op is False

    # neither the init, nor the results streamed during the run, release it
    analysis.set_results(jcoms.AnalysisResponse(), status=Analysis.Status.INITED)
    analysis.set_results(jcoms.AnalysisResponse(), False)
    await asyncio.sleep(0)
    assert analysis.needs_op is False

    # the run completing does
    analysis.set_results(jcoms.AnalysisResponse(), status=Analysis.Status.COMPLETE)
    await asyncio.sleep(0)
    assert analysis.needs_op is True

    analysis.op.set_result(None)
    await save
