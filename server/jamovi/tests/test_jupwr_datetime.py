"""jUPWR: date and time functions of computed variables (TIMEDIFF,
TIMEVALUE, DATETIMEVALUE, DATEVALUE without a format, YEAR ... MINUTE,
TIME)"""

import math

import pytest

from jamovi.server.compute import functions as f
from jamovi.server.datasetmodel import DataSetModel
from jamovi.server.dataset import ColumnType, DataType, MeasureType

MISSING_INT = -2147483648


def _nan(x):
    return isinstance(x, float) and math.isnan(x)


@pytest.mark.parametrize('start, end, unit, value', [
    ('8:15', '9:00', 'min', 45),
    ('08:15:30', '08:16', 's', 30),
    ('22:00', '02:00', 'h', 4),                      # over midnight: next day
    ('12:00', '12:00', 'min', 0),
    ('2026-10-22 08:15', '2026-10-22 10:45', 'h', 2.5),
    ('22.10.2026 22:00', '23.10.2026 02:00', 'h', 4),
    ('2026-10-22T10:00:00', '2026-10-22 08:00', 'h', -2),   # dates: exact, may be negative
    ('2026-10-01', '2026-10-22', 'd', 21),
    ('28.02.2024', '01.03.2024', 'd', 2),            # leap year
    ('28/02/2026', '01-03-2026', 'd', 1),
])
def test_timediff(start, end, unit, value):
    assert f.TIMEDIFF(0, start, end, unit=unit) == pytest.approx(value)


@pytest.mark.parametrize('start, end, unit', [
    ('8:15', '2026-10-22 09:00', 'min'),   # time of day against a date
    ('', '9:00', 'min'),
    ('8:15', 'jutro', 'min'),
    ('25:00', '9:00', 'min'),
    ('8:60', '9:00', 'min'),
    ('31.02.2026', '01.03.2026', 'd'),
    ('8:15', '9:00', 'tydzień'),
    ('10/22/2026', '10/23/2026', 'd'),     # US order is not a date
])
def test_timediff_missing(start, end, unit):
    assert _nan(f.TIMEDIFF(0, start, end, unit=unit))


def test_timevalue():
    assert f.TIMEVALUE(0, '8:15') == 495
    assert f.TIMEVALUE(0, '8:15', unit='h') == pytest.approx(8.25)
    assert f.TIMEVALUE(0, ' 00:00:30 ', unit='s') == 30
    assert f.TIMEVALUE(0, '2026-10-22 08:15') == 495
    assert _nan(f.TIMEVALUE(0, 'rano'))


def test_datevalue_and_datetimevalue():
    assert f.DATEVALUE(0, '1970-01-02') == 1
    assert f.DATEVALUE(0, '22.10.2026') == f.DATEVALUE(0, '2026-10-22')
    assert f.DATEVALUE(0, '2026-10-22 08:15') == f.DATEVALUE(0, '2026-10-22')
    assert f.DATEVALUE(0, '22/10/2026', '%d/%m/%Y') == f.DATEVALUE(0, '2026-10-22')
    assert f.DATEVALUE(0, 'x') == MISSING_INT
    assert f.DATEVALUE(0, '8:15') == MISSING_INT
    assert f.DATEVALUE(0, '2026-10-22', '%d.%m.%Y') == MISSING_INT
    assert f.DATETIMEVALUE(0, '1970-01-02 12:00') == pytest.approx(1.5)
    assert _nan(f.DATETIMEVALUE(0, '12:00'))


def test_parts():
    assert f.YEAR(0, '22.10.2026') == 2026
    assert f.MONTH(0, '2026-10-22') == 10
    assert f.DAY(0, '2026-10-22 08:15') == 22
    assert f.WEEKDAY(0, '2026-10-22') == 4      # Thursday
    assert f.WEEKDAY(0, '25.10.2026') == 7      # Sunday
    assert f.HOUR(0, '08:15') == 8
    assert f.MINUTE(0, '2026-10-22 08:15') == 15
    assert f.HOUR(0, '2026-10-22') == 0
    assert f.YEAR(0, '08:15') == MISSING_INT
    assert f.MONTH(0, '') == MISSING_INT


def test_time():
    assert f.TIME(0, 495.0) == '08:15'
    assert f.TIME(0, 8.25, unit='h') == '08:15'
    assert f.TIME(0, 90.5) == '01:30:30'
    assert f.TIME(0, 1500.0) == '25:00'
    assert f.TIME(0, -30.0) == '-00:30'
    assert f.TIME(0, float('nan')) == ''


def _text_column(model, name, values):
    column = model.append_column(name)
    column.change(data_type=DataType.TEXT, measure_type=MeasureType.NOMINAL)
    for i, value in enumerate(values):
        column.set_value(i, value)
    return column


def _computed(model, name, formula):
    column = model.append_column(name)
    column.column_type = ColumnType.COMPUTED
    column.formula = formula
    column.parse_formula()
    return column


def test_computed_column(dataset_model: DataSetModel):
    # elapsed time between two text columns, as a student would write it
    m = dataset_model
    m.set_row_count(4)
    _text_column(m, 'start', [ '8:15', '22:00', '', '12:00' ])
    _text_column(m, 'koniec', [ '9:00', '02:00', '10:00', 'potem' ])
    minutes = _computed(m, 'czas', 'TIMEDIFF(start, koniec)')
    hours = _computed(m, 'godz', "TIMEDIFF(start, koniec, unit='h')")
    m._recalc_all()

    assert minutes.formula_message == ''
    assert hours.formula_message == ''
    assert minutes.measure_type is MeasureType.CONTINUOUS
    assert [ minutes[i] for i in range(2) ] == [ 45, 240 ]
    assert hours[1] == pytest.approx(4)
    assert _nan(minutes[2])
    assert _nan(minutes[3])


def test_computed_column_bad_keyword(dataset_model: DataSetModel):
    m = dataset_model
    m.set_row_count(1)
    _text_column(m, 'start', [ '8:15' ])
    c = _computed(m, 'czas', "TIMEDIFF(start, start, jednostka='h')")
    assert c.formula_message != ''
