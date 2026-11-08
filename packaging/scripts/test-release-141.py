"""Exercise the real Docker C++/R engine, OMV migration, and plot exports.

Run inside an isolated container. --create-legacy requires distrACTION 1.4.0
compiled from commit b2090c5c; its output is the manual regression fixture.
"""
import argparse
import asyncio
import csv
import math
import tempfile
from pathlib import Path
import xml.etree.ElementTree as ET

from jamovi.server import jamovi_pb2 as pb
from jamovi.server.analyses import Analysis
from jamovi.server.options import write_value_to_pb
from jamovi.server.session import Session
from jamovi.server.utils import conf


def options(**values):
    result = pb.AnalysisOptions(hasNames=True)
    for name, value in values.items():
        result.names.append(name)
        write_value_to_pb(value, result.options.add())
    return result


async def complete(analysis):
    async with asyncio.timeout(60):
        while not analysis.complete or analysis.status not in (Analysis.Status.COMPLETE, Analysis.Status.ERROR):
            await asyncio.sleep(0.02)
    assert analysis.status == Analysis.Status.COMPLETE, str(analysis.results)
    assert not analysis.results.results.error.message, str(analysis.results)


def table_cell(analysis, table, column, row=0):
    element = next(e for e in analysis.results.results.group.elements if e.name == table)
    col = next(c for c in element.table.columns if c.name == column)
    cell = col.cells[row]
    return getattr(cell, cell.WhichOneof('cellType'))


async def run(args):
    with tempfile.TemporaryDirectory(prefix='jupwr-141-') as work:
        conf.set('recovery_path', str(Path(work) / 'recovery'))
        conf.set('recovery_interval', 0)
        conf.set('backend', 'none')
        session = Session(work, 'release-141')
        await session.start()
        try:
            instance = await session.create()
            await instance.open(str(args.data))
            if args.boundaries_only:
                for i, p in enumerate((0, 1)):
                    analysis = instance.analyses.create(2 + 2 * i, 'FDistribution', 'distrACTION', options(
                        dp1=1, dp2=10, QuantileFunction=True, p=p))
                    analysis.rerun()
                    await complete(analysis)
                    assert table_cell(analysis, 'Outputs', 'QuantileResultColumn') == f'x1 = {"Inf" if p else "0"}'
                    target = Path(work) / f'F-p{p}.png'
                    await analysis.save(str(target), 'plot')
                    assert target.stat().st_size > 100
                analysis = instance.analyses.create(6, 'WeibullDistribution', 'distrACTION', options(
                    dp1=0.001, dp2=1, DistributionFunction=True, x1=1,
                    QuantileFunction=True, QuantileFunctionType='cumulative',
                    showMean=True, showVariance=True))
                analysis.rerun()
                await complete(analysis)
                assert table_cell(analysis, 'Outputs', 'DistributionResultColumn') == 'P = 0.632'
                plot = next(e for e in analysis.results.results.group.elements if e.name == 'plot')
                assert 'możliwości liczbowe' in plot.error.message, plot
                analysis = instance.analyses.create(8, 'anovarm', 'jANOVA', options(
                    dep='wynik', subject='id', within=['czas'], nonpar=True, showPairs=True))
                analysis.rerun()
                await complete(analysis)
                q = sorted(table_cell(analysis, 'npPairs', 'stat', row) for row in range(3))
                expected = [math.sqrt(12), math.sqrt(12), 2 * math.sqrt(12)]
                assert all(abs(a - b) < 1e-9 for a, b in zip(q, expected)), q
                print('C++/R: F endpoints, extreme Weibull and Nemenyi q OK')
                return
            if args.percentiles_only:
                analysis = instance.analyses.create(2, 'ilosciowe', 'jEksplor', options(
                    vars=['x'], pc=True, pcValues='2,5', decSymbol='.'))
                analysis.rerun()
                await complete(analysis)

                def columns():
                    element = next(e for e in analysis.results.results.group.elements if e.name == 'desc')
                    return {column.name: column for column in element.table.columns}

                assert {'pc2', 'pc5'} <= columns().keys()
                # Use the real profile-setting event path, with no edit to the
                # analysis options or percentile string between the two runs.
                session.apply_settings({'decSymbol': ','})
                await complete(analysis)
                cols = columns()
                assert 'pc2.5' in cols, cols.keys()
                assert not {'pc2', 'pc5'} & cols.keys(), cols.keys()
                assert cols['pc2.5'].title == 'P2,5'
                with args.data.open() as source:
                    values = sorted(float(row['x']) for row in csv.DictReader(source))
                for p in (2.5,):
                    position = (len(values) - 1) * p / 100
                    i = int(position)
                    expected = values[i] + (position - i) * (values[i + 1] - values[i])
                    assert abs(table_cell(analysis, 'desc', f'pc{p}') - expected) < 1e-9
                session.apply_settings({'decSymbol': '.'})
                await complete(analysis)
                assert {'pc2', 'pc5'} <= columns().keys()
                print('Percentiles: decimal setting, labels, values and cache invalidation OK')
                return
            if args.plots_only:
                cases = [
                    ('jmvbar', dict(mode='categorical', catvar='grupa')),
                    ('jmvhist', dict(var='x', group='grupa')),
                    ('discbar', dict(var='licznosc')),
                    ('dens', dict(var='x', group='grupa')),
                    ('ridge', dict(var='x', group='grupa')),
                    ('mosaic', dict(x='grupa', y='warstwa')),
                    ('stackbar', dict(x='grupa', segments='warstwa')),
                    ('circbar', dict(x='grupa', y='x')),
                    ('lollipop', dict(x='grupa', y='x')),
                    ('pareto', dict(x='grupa')),
                    ('heatmap', dict(x='grupa', y='warstwa', value='x')),
                    ('corrgram', dict(vars=['x', 'y', 'z'])),
                    ('hexbin', dict(x='x', y='y')),
                    ('radar', dict(vars=['x', 'y', 'z'], group='grupa')),
                    ('treemap', dict(group='grupa', group2='warstwa')),
                    ('waffle', dict(group='grupa', group2='warstwa')),
                    ('meanprofile', dict(vars=['x', 'y', 'z'], group='grupa')),
                ]
                for i, (name, values) in enumerate(cases):
                    analysis = instance.analyses.create(2 + 2 * i, name, 'scatr', options(**values))
                    analysis.rerun()
                    await complete(analysis)
                    plot = next(e for e in analysis.results.results.group.elements if e.name == 'plot')
                    assert plot.image.path.endswith('.svg'), (name, plot)
                    resource = Path(instance.project.instance_path) / plot.image.path
                    svg = ET.parse(resource).getroot()
                    assert svg.tag.endswith('svg'), name
                    # Cairo may rasterize a colour-bar gradient, but the plot
                    # must contain vector paths rather than only a bitmap.
                    assert svg.findall('.//{http://www.w3.org/2000/svg}path'), name
                    for ext in ('png', 'svg', 'pdf'):
                        target = Path(work) / f'{name}.{ext}'
                        await asyncio.wait_for(analysis.save(str(target), 'plot'), 60)
                        assert target.stat().st_size > 100, target
                    print('Vector resource and exports:', name)
                target = Path(work) / 'vector-plots.omv'
                await instance.save({'path': str(target), 'overwrite': True})
                reopened = await session.create()
                await reopened.open(str(target))
                for analysis in reopened.analyses:
                    if analysis.ns != 'scatr':
                        continue
                    analysis.rerun()
                    await complete(analysis)
                    plot = next(e for e in analysis.results.results.group.elements if e.name == 'plot')
                    assert plot.image.path.endswith('.svg'), (analysis.name, plot)
                print('Vector plots: OMV save/reopen/recalculate OK')
                return
            if args.create_legacy:
                analysis = instance.analyses.create(2, 'BinomialDistribution', 'distrACTION', options(
                    empVar='kontrole', dp1=12, dp2=0.15, DistributionFunction=True,
                    DistributionFunctionType='is', x1=0, empParams='estimated',
                    empBand=True, empChiSq=True))
                analysis.rerun()
                await complete(analysis)
                await instance.save({'path': str(args.create_legacy), 'overwrite': True})
                print('Legacy OMV saved:', args.create_legacy)
                return

            cases = [
                ('BinomialDistribution', dict(empVar='kontrole', dp1=12, dp2=0.15)),
                ('PoissonDistribution', dict(empVar='kontrole', dp1=2)),
                ('GeometricDistribution', dict(empVar='kontrole', dp1=0.25)),
                ('NegBinomialDistribution', dict(empVar='kontrole', dp1=3, dp2=0.2,
                                                 RandomVariable='failures')),
                ('Normaldistribution', dict(empVar='czas', dp1=3, dp2=1)),
                ('ExponentialDistribution', dict(empVar='czas', dp1=0.5)),
                ('GammaDistribution', dict(empVar='czas', dp1=2, dp2=0.5)),
                ('WeibullDistribution', dict(empVar='czas', dp1=1.5, dp2=3)),
            ]
            for i, (name, values) in enumerate(cases):
                analysis = instance.analyses.create(2 + 2 * i, name, 'distrACTION', options(**values))
                analysis.rerun()
                await complete(analysis)
                for ext in ('png', 'svg', 'pdf'):
                    target = Path(work) / f'{name}.{ext}'
                    await asyncio.wait_for(analysis.save(str(target), 'plot'), 60)
                    assert target.stat().st_size > 100, target
                print('C++/R and exports:', name)

            legacy = await session.create()
            await legacy.open(str(args.legacy))
            analysis = next(a for a in legacy.analyses if a.name == 'BinomialDistribution')
            analysis.rerun()
            await complete(analysis)
            assert table_cell(analysis, 'Outputs', 'DistributionResultColumn') == 'P = 0.142'
            assert table_cell(analysis, 'EmpTable', 'obs', -1) == 150
            target = Path(work) / 'migrated.omv'
            await legacy.save({'path': str(target), 'overwrite': True})
            reopened = await session.create()
            await reopened.open(str(target))
            migrated = next(a for a in reopened.analyses if a.name == 'BinomialDistribution')
            migrated.rerun()
            await complete(migrated)
            assert table_cell(migrated, 'Outputs', 'DistributionResultColumn') == 'P = 0.142'
            assert table_cell(migrated, 'EmpTable', 'obs', -1) == 150
            print('Legacy 1.4.0 OMV: open, recalculate, save, reopen OK')
        finally:
            session.stop()
            await asyncio.wait_for(session.wait_ended(), 20)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--data', type=Path, required=True)
    parser.add_argument('--legacy', type=Path)
    parser.add_argument('--create-legacy', type=Path)
    parser.add_argument('--plots-only', action='store_true')
    parser.add_argument('--percentiles-only', action='store_true')
    parser.add_argument('--boundaries-only', action='store_true')
    args = parser.parse_args()
    if not args.create_legacy and not args.legacy and not args.plots_only and not args.percentiles_only and not args.boundaries_only:
        parser.error('--legacy is required unless creating a legacy fixture')
    asyncio.run(run(args))
