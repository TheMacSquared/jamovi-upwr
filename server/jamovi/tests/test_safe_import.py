"""Failed source reads must preserve document data, formulas and undo history."""

import asyncio
import math
from pathlib import Path
import threading

import pytest

from jamovi.server import instance as instance_code
from jamovi.server import formatio
from jamovi.server import jamovi_pb2 as pb
from jamovi.server.dataset import ColumnType
from jamovi.tests.test_instance import FakeComs


async def target_document(instance):
    await instance.open('')
    coms = FakeComs()
    instance.set_coms(coms)
    request = pb.DataSetRR(op=pb.SET, incData=True)
    block = request.data.add(rowStart=0, columnStart=0, rowCount=2, columnCount=2)
    for value in [7, 9]:
        block.values.add().i = value
    for value in ['łąka', 'żółw']:
        block.values.add().s = value
    await instance.on_request(request)
    assert not coms.errors
    model = instance.project.get_dataset()
    derived = model.append_column('derived')
    derived.column_type = ColumnType.COMPUTED
    derived.formula = 'A * 2'
    derived.parse_formula()
    model._recalc_all()
    return coms


def snapshot(instance):
    dataset = instance.project.get_dataset()
    def value(cell):
        return 'NaN' if isinstance(cell, float) and math.isnan(cell) else cell
    return dict(
        rows=dataset.row_count,
        columns=[(c.id, c.name, c.import_name, c.column_type, c.data_type,
                  c.measure_type, c.formula, list(c.levels),
                  [value(c[r]) for r in range(dataset.row_count)]) for c in dataset],
        edited=instance.project.is_edited,
        path=instance.project.path,
        title=instance.project.title,
        analyses=[a.serialize() for a in instance.analyses],
        undo=(instance._controller().mod_tracker.count, instance._controller().mod_tracker.position),
    )


def csv(path, values=(1, 2)):
    path.write_text('A,B\n' + ''.join(f'{value},pole {value}\n' for value in values), encoding='utf-8')
    return str(path)


def request(paths):
    return pb.OpenRequest(op=pb.OpenRequest.IMPORT_REPLACE, filePaths=paths)


def track_buffers(monkeypatch, tmp_path):
    directories, stores = [], []
    temporary_directory = instance_code.TemporaryDirectory
    create_store = instance_code.StoreFactory.create

    def scratch(*args, **kwargs):
        result = temporary_directory(*args, dir=tmp_path, **kwargs)
        directories.append(Path(result.name))
        return result

    def create(*args):
        store = create_store(*args)
        original_close = store.close
        record = {'closed': False}
        def close():
            record['closed'] = True
            original_close()
        store.close = close
        stores.append(record)
        return store

    monkeypatch.setattr(instance_code, 'TemporaryDirectory', scratch)
    monkeypatch.setattr(instance_code.StoreFactory, 'create', create)
    return directories, stores


@pytest.mark.asyncio
@pytest.mark.parametrize('position', [0, 1, 2])
@pytest.mark.parametrize('failure', ['missing', 'unsupported', 'corrupt-omv', 'partial-read'])
async def test_failed_file_anywhere_preserves_entire_document(instance, tmp_path, monkeypatch, position, failure):
    coms = await target_document(instance)
    before = snapshot(instance)
    paths = [csv(tmp_path / f'good-{index}.csv') for index in range(3)]
    bad = tmp_path / ('bad.omv' if failure == 'corrupt-omv' else 'bad.csv')
    if failure == 'unsupported':
        bad = tmp_path / 'bad.unknown'
        bad.write_text('unsupported', encoding='utf-8')
    elif failure == 'corrupt-omv':
        bad.write_bytes(b'not an omv archive')
    elif failure == 'partial-read':
        bad.write_text('A\n1\n', encoding='utf-8')
        read = formatio.read_dataset
        def fail(model, path, *args, **kwargs):
            if path == str(bad):
                model.append_column('partial')
                model.set_row_count(1)
                raise OSError('read failed halfway')
            return read(model, path, *args, **kwargs)
        monkeypatch.setattr(formatio, 'read_dataset', fail)
    paths[position] = str(bad)
    directories, stores = track_buffers(monkeypatch, tmp_path)
    await instance.on_request(request(paths))
    assert len(coms.errors) == 1
    assert snapshot(instance) == before
    assert all(record['closed'] for record in stores)
    assert directories and all(not directory.exists() for directory in directories)


@pytest.mark.asyncio
async def test_empty_selection_preserves_data_and_history(instance):
    coms = await target_document(instance)
    before = snapshot(instance)
    await instance.on_request(request([]))
    assert len(coms.errors) == 1
    assert snapshot(instance) == before


@pytest.mark.asyncio
async def test_combined_row_limit_is_checked_before_replacement(instance, tmp_path, monkeypatch):
    coms = await target_document(instance)
    before = snapshot(instance)
    model = instance.project.get_dataset()
    monkeypatch.setitem(model._perms.dataset, 'maxRows', 3)
    paths = [csv(tmp_path / f'part-{index}.csv') for index in range(2)]
    await instance.on_request(request(paths))
    assert len(coms.errors) == 1
    assert snapshot(instance) == before


@pytest.mark.asyncio
async def test_successful_multi_import_keeps_sources_alive_and_recalculates(instance, tmp_path, monkeypatch):
    coms = await target_document(instance)
    paths = [csv(tmp_path / 'pierwszy.csv', (1, 2)), csv(tmp_path / 'drugi.csv', (3,))]
    directories, stores = track_buffers(monkeypatch, tmp_path)
    await instance.on_request(request(paths))
    assert not coms.errors
    dataset = instance.project.get_dataset()
    assert dataset.row_count == 3
    assert [dataset['A'][r] for r in range(3)] == [1, 2, 3]
    assert [dataset['derived'][r] for r in range(3)] == [2, 4, 6]
    assert [dataset['source'][r] for r in range(3)] == ['pierwszy', 'pierwszy', 'drugi']
    assert not instance._controller().mod_tracker.can_undo
    assert all(record['closed'] for record in stores)
    assert all(not directory.exists() for directory in directories)


@pytest.mark.asyncio
async def test_valid_empty_file_can_intentionally_replace_data(instance, tmp_path):
    coms = await target_document(instance)
    await instance.on_request(request([csv(tmp_path / 'empty.csv', ())]))
    assert not coms.errors
    assert instance.project.get_dataset().row_count == 0


@pytest.mark.asyncio
async def test_repeated_cancellation_waits_for_reader_before_closing_buffers(instance, tmp_path, monkeypatch):
    coms = await target_document(instance)
    before = snapshot(instance)
    source = csv(tmp_path / 'delayed.csv')
    started, release = threading.Event(), threading.Event()
    read = formatio.read_dataset

    def delayed(*args, **kwargs):
        started.set()
        assert release.wait(5)
        return read(*args, **kwargs)

    monkeypatch.setattr(formatio, 'read_dataset', delayed)
    directories, stores = track_buffers(monkeypatch, tmp_path)
    task = asyncio.create_task(instance.on_request(request([source])))
    assert await asyncio.to_thread(started.wait, 2)
    try:
        task.cancel()
        await asyncio.sleep(.01)
        task.cancel()
        await asyncio.sleep(.01)
        assert not task.done()
        assert not any(record['closed'] for record in stores)
        assert snapshot(instance) == before
    finally:
        release.set()
        await asyncio.gather(task, return_exceptions=True)
    assert task.cancelled()
    assert snapshot(instance) == before
    assert not coms.errors
    assert all(record['closed'] for record in stores)
    assert all(not directory.exists() for directory in directories)
