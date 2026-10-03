"""Tests for scripts/sync_alda_from_psnd.py, on a fake psnd tree."""

import hashlib
import importlib.util
from pathlib import Path

import pytest

SCRIPT = Path(__file__).resolve().parent.parent / "scripts" / "sync_alda_from_psnd.py"
spec = importlib.util.spec_from_file_location("sync_alda_from_psnd", SCRIPT)
sync_mod = importlib.util.module_from_spec(spec)
spec.loader.exec_module(sync_mod)

SyncError = sync_mod.SyncError
Manifest = sync_mod.Manifest


def write(path: Path, text: str) -> Path:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text)
    return path


@pytest.fixture
def trees(tmp_path):
    psnd, dest = tmp_path / "psnd", tmp_path / "dest"
    write(psnd / "impl/ast.c", "int ast;\n")
    write(psnd / "impl/context.c", "#include \"shared.h\"\nint ctx;\n")
    write(psnd / "impl/scala.c", "int scala;\n")
    write(psnd / "scheduler.c", "int sched;\n")
    write(psnd / "examples/a.alda", "piano: c\n")
    write(psnd / "ref/a.expected", "NOTE 60\n")
    write(psnd / "ref/README.md", "psnd paths\n")
    dest.mkdir()
    return psnd, dest


def manifest(psnd: Path) -> Manifest:
    m = Manifest()
    m.verbatim["impl/ast.c"] = "lib/ast.c"
    m.adapted["impl/context.c"] = (
        "lib/context.c",
        lambda t: sync_mod.replace_once(t, '#include "shared.h"\n', "", "context.c"))
    m.watched["scheduler.c"] = hashlib.sha256((psnd / "scheduler.c").read_bytes()).hexdigest()
    m.ignored = ["impl/scala.c"]
    m.data_dirs = [("examples", "docs/examples", "*.alda", ()),
                   ("ref", "docs/ref", "*", ("README.md",))]
    m.audited_dirs = [("impl", "*")]
    return m


def test_sync_writes_verbatim_adapted_and_data(trees):
    psnd, dest = trees
    report = sync_mod.sync(psnd, dest, manifest(psnd), write=True)

    assert (dest / "lib/ast.c").read_text() == "int ast;\n"
    assert (dest / "lib/context.c").read_text() == "int ctx;\n"
    assert (dest / "docs/examples/a.alda").read_text() == "piano: c\n"
    assert (dest / "docs/ref/a.expected").read_text() == "NOTE 60\n"
    assert not (dest / "docs/ref/README.md").exists()
    assert len(report.changed) == 4
    assert report.warnings == []


def test_second_sync_changes_nothing(trees):
    psnd, dest = trees
    sync_mod.sync(psnd, dest, manifest(psnd), write=True)
    report = sync_mod.sync(psnd, dest, manifest(psnd), write=True)
    assert report.changed == []
    assert report.unchanged == 4


def test_check_mode_reports_without_writing(trees):
    psnd, dest = trees
    report = sync_mod.sync(psnd, dest, manifest(psnd), write=False)
    assert "lib/ast.c" in report.changed
    assert not (dest / "lib").exists()


def test_local_edit_is_reported_as_drift(trees):
    psnd, dest = trees
    sync_mod.sync(psnd, dest, manifest(psnd), write=True)
    write(dest / "lib/ast.c", "int ast; /* local edit */\n")
    report = sync_mod.sync(psnd, dest, manifest(psnd), write=False)
    assert report.changed == ["lib/ast.c"]


def test_missing_anchor_raises(trees):
    psnd, dest = trees
    write(psnd / "impl/context.c", "int ctx;\n")  # upstream dropped the include
    with pytest.raises(SyncError, match="context.c"):
        sync_mod.sync(psnd, dest, manifest(psnd), write=True)


def test_missing_upstream_file_raises(trees):
    psnd, dest = trees
    (psnd / "impl/ast.c").unlink()
    with pytest.raises(SyncError, match="impl/ast.c"):
        sync_mod.sync(psnd, dest, manifest(psnd), write=True)


def test_watched_file_change_warns(trees):
    psnd, dest = trees
    m = manifest(psnd)
    write(psnd / "scheduler.c", "int sched2;\n")
    report = sync_mod.sync(psnd, dest, m, write=False)
    assert any("scheduler.c changed upstream" in w for w in report.warnings)


def test_new_upstream_file_warns(trees):
    psnd, dest = trees
    write(psnd / "impl/new.c", "int n;\n")
    report = sync_mod.sync(psnd, dest, manifest(psnd), write=False)
    assert any("impl/new.c is new upstream" in w for w in report.warnings)


def test_destination_only_data_file_warns_and_is_kept(trees):
    psnd, dest = trees
    stale = write(dest / "docs/examples/old.alda", "piano: d\n")
    report = sync_mod.sync(psnd, dest, manifest(psnd), write=True)
    assert stale.exists()
    assert any("docs/examples/old.alda is not in psnd" in w for w in report.warnings)


def test_replace_once_rejects_duplicate_anchor():
    with pytest.raises(SyncError, match="found 2"):
        sync_mod.replace_once("a a", "a", "b", "f")


def test_replace_between_requires_end_after_start():
    with pytest.raises(SyncError, match="end anchor"):
        sync_mod.replace_between("END START", "START", "END", "", "f")
    assert sync_mod.replace_between("x START y END z", "START", "END", "-", "f") == "x -END z"


def test_test_shared_suite_adaptation_rejects_new_psnd_reference():
    text = ("Check psnd's Alda output test_alda_shared_suite DIR and psnd numbers them "
            "are read from psnd's events read_psnd Reading psnd's events , psnd % "
            "must be in effect in psnd's docs/dev/conformance.md the method\n"
            "/* new psnd comment */\n")
    with pytest.raises(SyncError, match="new psnd references"):
        sync_mod.adapt_test_shared_suite(text)


def test_main_rejects_missing_psnd(tmp_path, capsys):
    assert sync_mod.main(["--psnd", str(tmp_path / "none"), "--dest", str(tmp_path)]) == 2
    assert "no psnd Alda sources" in capsys.readouterr().err
