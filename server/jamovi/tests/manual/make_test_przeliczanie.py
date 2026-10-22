"""jUPWR: synthetic .omv for manual tests of rule/IF recalculation (task 24,
point 13) -- 10 000 rows, transformed variables with rules, computed IF
columns, an inactive filter; the column descriptions say what to test.

Regenerate in the Docker image (needs ../conftest.py next to it), e.g.:
  docker run --rm --entrypoint sh -v <this dir>:/rt:ro -v <out dir>:/out \
    -v <tests dir>/conftest.py:/rt/conftest.py:ro jupwr/jupwr:<ver> \
    -c 'cd /rt && python3 -m pytest /rt/make_test_przeliczanie.py -q -s -p no:cacheprovider'
It writes /out/test_przeliczanie.omv and checks that reopening the file gives
the same values and levels. (Not named test_*.py, so the server test suite
does not run it.)"""
import math, random

import pytest

from jamovi.server.dataset import ColumnType, DataType, MeasureType

N = 10000
OUT = '/out/test_przeliczanie.omv'


class FakeComs:
    def __init__(self): self.sent = [ ]
    def send(self, message=None, instance_id=None, response_to=None, complete=True, progress=(0, 0), status=None): pass
    def send_error(self, message=None, cause=None, instance_id=None, response_to=None): pass
    def add_close_listener(self, listener): pass
    def remove_close_listener(self, listener): pass


async def _run(stream):
    async for _ in stream:
        pass
    return await stream


def rules_20():
    f = '"k20"'
    for i in range(19, 0, -1):
        f = 'IF(x < {}, "k{:02d}", {})'.format(i * 5, i, f)
    return f


_created = [ 0 ]


def _new(m, name):
    # inserted in order at the front: appending would put the columns
    # after the blank (virtual) columns of a new data set
    c = m.insert_column(_created[0], name)
    _created[0] += 1
    return c


def column(m, name, dtype, mtype, desc):
    c = _new(m, name)
    c.column_type = ColumnType.DATA
    c.change(data_type=dtype, measure_type=mtype)
    c.description = desc
    return c


def recoded(m, name, source, formula, mtype, desc):
    t = m.append_transform(name + ' (reguły)')
    t.formula = [ formula ]
    t.measure_type = mtype
    t.description = desc
    c = _new(m, name)
    c.column_type = ColumnType.RECODED
    c.parent_id = source.id
    c.transform = t.id
    c.description = desc
    return c


def computed(m, name, formula, desc):
    c = _new(m, name)
    c.column_type = ColumnType.COMPUTED
    c.formula = formula
    c.description = desc
    return c


@pytest.mark.asyncio
async def test_make(session):
    inst = await session.create()
    await _run(inst.open(''))
    inst.set_coms(FakeComs())
    m = inst._project.get_dataset()
    print('BLANK', [ (c.name, c.column_type, c.is_virtual) for c in m ])

    random.seed(2026)
    m.set_row_count(N)
    idc = column(m, 'id', DataType.INTEGER, MeasureType.ID, 'Numer wiersza.')
    x = column(m, 'x', DataType.DECIMAL, MeasureType.CONTINUOUS,
               'Wartość 0–100, co 97. wiersz pusty. TEST: zmień kilka komórek x i zobacz, jak szybko przeliczają się kolumny z regułami (x_20kat ma 20 reguł na 10 000 wierszy).')
    k = column(m, 'k', DataType.TEXT, MeasureType.NOMINAL, 'Kategoria: kot, pies, mysz; co 101. wiersz pusty.')
    start = column(m, 'start', DataType.TEXT, MeasureType.NOMINAL, 'Godzina rozpoczęcia (tekst, np. 8:15).')
    koniec = column(m, 'koniec', DataType.TEXT, MeasureType.NOMINAL, 'Godzina zakończenia; część po północy (np. 22:30 → 01:10).')
    for i in range(N):
        idc.set_value(i, i + 1)
        x.set_value(i, float('nan') if i % 97 == 5 else round(random.uniform(0, 100), 1))
        k.set_value(i, '' if i % 101 == 7 else random.choice([ 'kot', 'pies', 'mysz' ]))
        h = random.randint(6, 23); mi = random.choice([ 0, 15, 30, 45 ])
        d = random.randint(15, 240)
        e = (h * 60 + mi + d) % (24 * 60)
        start.set_value(i, '{}:{:02d}'.format(h, mi))
        koniec.set_value(i, '{:02d}:{:02d}'.format(e // 60, e % 60))

    recoded(m, 'x_20kat', x, rules_20(), MeasureType.NOMINAL,
            '20 reguł (k01: x < 5, …, k20: reszta). TEST 1 i 2: szybkość po edycji x; zmiana kategorii w edytowanym wierszu; lista poziomów w kolejności reguł; pusty x → pusta komórka.')
    recoded(m, 'x_poziom', x, 'IF(x < 30, "nisko", IF(x < 60, "średnio", "wysoko"))', MeasureType.ORDINAL,
            'Porządkowa: nisko < średnio < wysoko. TEST 2: kolejność poziomów zgodna z regułami; edytuj reguły (np. próg 60 → 50) i sprawdź komórki.')
    recoded(m, 'k_grupa', k, 'IF(k == "kot", "domowe", IF(k == "pies", "domowe", "inne"))', MeasureType.NOMINAL,
            'Reguły na tekście: kot, pies → domowe, reszta → inne. TEST 2: zmień w k ostatniego „psa”/„kota” i zobacz, czy poziom znika lub wraca.')
    recoded(m, 'x_z', x, 'IF(Z(x) > 0.5, "powyżej", IF(Z(x) < -0.5, "poniżej", "średnio"))', MeasureType.NOMINAL,
            'Reguła z funkcją kolumnową Z(x). TEST 3: włącz filtr „Filter 1” (x < 70) — Z liczy się tylko z widocznych wierszy, kategorie się przesuną.')

    computed(m, 'c_if', 'IF(x > 50, "duże", "małe")',
             'TEST 4: zwykła zmienna obliczana z IF; pusty x → pusta komórka.')
    computed(m, 'c_ln', 'IF(x > 50, LN(x - 50), 0)',
             'TEST 4 (nowe zachowanie 1.2.0): dla x ≤ 50 ma być 0 (wcześniej puste, bo liczony był też LN liczby ujemnej); dla x > 50 = ln(x − 50).')
    computed(m, 'c_zagn', 'IF(x == NA, "brak", IF(x < 50, IF(k == "kot", "mały kot", "małe inne"), "duże"))',
             'TEST 4: zagnieżdżone IF i warunek na brak danych (x == NA → „brak”).')
    computed(m, 'c_czas', 'TIMEDIFF(start, koniec)',
             'Czas w minutach (funkcje dat 1.2.0); koniec przed startem = następny dzień.')

    f = m.insert_column(0)
    f.column_type = ColumnType.FILTER
    f.formula = 'x < 70'
    f.description = 'TEST 3: filtr wyłączony; włącz go, aby sprawdzić zmienne z regułami i x_z przy filtrze.'
    f.active = False
    m.set_column_name(f, 'Filtr: x < 70')

    for t in m.transforms:
        t.parse_formula()
    for c in m:
        c.set_needs_parse()
    for c in m:
        c.parse_formula()
    m.refresh_filter_state()
    m._recalc_all()

    # the default columns A, B, C of a new data set (empty)
    blank = [ c.id for c in m if c.name in ('A', 'B', 'C') and not c.is_virtual ]
    if blank:
        m.delete_columns_by_id(blank)
    print('FINAL', [ (c.name, c.column_type, c.is_virtual) for c in m ][-8:])

    def snapshot(model):
        out = { }
        for c in model:
            if c.is_virtual or c.column_type == ColumnType.NONE:
                continue
            vals = [ c[i] for i in range(model.row_count) ]
            out[c.name] = ([ 'NaN' if isinstance(v, float) and v != v else v for v in vals ], [ l[1] for l in c.levels ])
        return out

    before = snapshot(m)
    for c in m:
        print('COL', c.name, c.column_type, c.data_type, c.measure_type, repr(c.formula_message)[:60],
              'levels', [ l[1] for l in c.levels ][:6], 'v', [ c[i] for i in range(3) ])
    await _run(inst.save({ 'path': OUT, 'overwrite': True }))
    inst.close()

    # reopen the saved file in a new instance
    again = await session.create()
    again.set_coms(FakeComs())
    await _run(again.open(OUT))
    m2 = again._project.get_dataset()
    print('REOPEN rows', m2.row_count, 'cols', [ c.name for c in m2 ])
    after = snapshot(m2)
    diffs = [ n for n in before if before[n] != after.get(n) ]
    print('DIFF', diffs if diffs else 'none -- every value and level identical after reopening')
    assert not diffs
    for name in [ 'x_20kat', 'x_poziom', 'c_ln', 'c_czas' ]:
        c = m2.get_column_by_name(name)
        print('REOPEN', name, c.column_type, repr(c.formula_message), [ l[1] for l in c.levels ][:5], [ c[i] for i in range(4) ])
