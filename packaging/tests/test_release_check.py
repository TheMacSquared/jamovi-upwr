"""Exercise the release gate with isolated metadata and deliberate inconsistencies."""
import pathlib
import shutil
import subprocess
import tempfile
import unittest

ROOT = pathlib.Path(__file__).resolve().parents[2]


class ReleaseCheckTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="jupwr-release-")
        self.addCleanup(self.temp.cleanup)
        self.root = pathlib.Path(self.temp.name)
        paths = [
            "packaging/scripts/release-check.sh", "client/common/jupwr.ts",
            "docker-compose.yaml", "docker/jamovi-Dockerfile",
            "packaging/scripts/macos/20-modules.sh", "packaging/scripts/windows/build.ps1",
            "packaging/scripts/windows/jUPWR.nsi", "CHANGELOG.md", "packaging/MODULES.md", "version",
        ]
        for metadata in ROOT.glob("*/jamovi/0000.yaml"):
            description = metadata.parents[1] / "DESCRIPTION"
            if description.exists():
                paths.extend([metadata.relative_to(ROOT), description.relative_to(ROOT)])
        for path in paths:
            dest = self.root / path
            dest.parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(ROOT / path, dest)

    def run_check(self, *args):
        return subprocess.run(
            ["bash", str(self.root / "packaging/scripts/release-check.sh"), *args],
            capture_output=True, text=True, timeout=20,
        )

    def replace(self, path, old, new):
        target = self.root / path
        content = target.read_text()
        self.assertIn(old, content)
        target.write_text(content.replace(old, new))

    def test_metadata_passes_without_local_artifacts(self):
        result = self.run_check("--metadata-only")
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertNotEqual(self.run_check().returncode, 0)

    def test_docker_version_is_mandatory(self):
        self.replace("docker-compose.yaml", "image: jupwr/jupwr:", "image: jupwr/wrong:")
        self.assertNotEqual(self.run_check("--metadata-only").returncode, 0)

    def test_windows_version_is_mandatory(self):
        self.replace("packaging/scripts/windows/jUPWR.nsi", '!define VERSION', '!define WRONG')
        self.assertNotEqual(self.run_check("--metadata-only").returncode, 0)

    def test_module_lists_are_mandatory(self):
        self.replace("packaging/scripts/macos/20-modules.sh", "MODULES=(jmv ", "MODULES=(")
        self.assertNotEqual(self.run_check("--metadata-only").returncode, 0)

    def test_changelog_is_mandatory(self):
        (self.root / "CHANGELOG.md").write_text("")
        self.assertNotEqual(self.run_check("--metadata-only").returncode, 0)

    def test_module_versions_are_mandatory(self):
        self.replace("jCI/DESCRIPTION", "Version:", "Wrong:")
        self.assertNotEqual(self.run_check("--metadata-only").returncode, 0)

    def test_matrix_match_must_belong_to_the_module(self):
        self.replace("packaging/MODULES.md", "### jRISK", "### OtherModule")
        result = self.run_check("--metadata-only")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("brak wiersza macierzy", result.stdout)


if __name__ == "__main__":
    unittest.main()
