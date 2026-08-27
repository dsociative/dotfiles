"""Tests for scripts/.local/bin/waybar-windows (pure functions; no waybar/Hyprland needed).

Run: python -m unittest tests/test_waybar_windows.py
"""
import importlib.machinery
import importlib.util
import os
import pathlib
import subprocess
import tempfile
import time
import sys
import unittest

sys.dont_write_bytecode = True  # never leave __pycache__ next to the stowed script

SCRIPT = pathlib.Path(__file__).resolve().parents[1] / "scripts" / ".local" / "bin" / "waybar-windows"

ROBOT, SSH, COG, TERM = "\U000F06A9", "\U000F08C0", "\uf013", "\uf120"
DIM_OPEN = '<span alpha="50%">'


def load():
    loader = importlib.machinery.SourceFileLoader("waybar_windows", str(SCRIPT))
    spec = importlib.util.spec_from_loader("waybar_windows", loader)
    mod = importlib.util.module_from_spec(spec)
    loader.exec_module(mod)
    return mod


W = load()
DARK = W.PALETTES["dark"]
HOME = os.path.expanduser("~")


def kitty(title, **extra):
    c = {"address": "0x1", "mapped": True, "hidden": False, "workspace": {"id": 1, "name": "1"},
         "class": "kitty", "title": title, "pid": 1, "focusHistoryID": 5, "at": [0, 0]}
    c.update(extra)
    return c


class ClipTest(unittest.TestCase):
    def test_short_string_unchanged(self):
        self.assertEqual(W.clip("cargo build", 18), "cargo build")

    def test_long_string_cut_to_n_code_points_with_ellipsis(self):
        out = W.clip("acp-session-rust-poc", 18)
        self.assertEqual(out, "acp-session-rust-…")
        self.assertEqual(len(out), 18)

    def test_cyrillic_counted_by_code_points_not_bytes(self):
        out = W.clip("Виртуальная машина расширение ОЗУ", 18)
        self.assertEqual(len(out), 18)
        self.assertTrue(out.endswith("…"))


class ShortDirTest(unittest.TestCase):
    def test_home_is_tilde(self):
        self.assertEqual(W.short_dir(HOME), "~")

    def test_basename_for_other_dirs(self):
        self.assertEqual(W.short_dir(HOME + "/study/kaiten-cli"), "kaiten-cli")

    def test_root(self):
        self.assertEqual(W.short_dir("/"), "/")


class KittyLabelTest(unittest.TestCase):
    def test_claude_busy_robot_topic_and_dim_cwd(self):
        out = W.label(kitty("◐ acp-session-rust-poc"), HOME + "/study", DARK)
        self.assertEqual(out, f"{ROBOT} acp-session-rust-… {DIM_OPEN}study</span>")

    def test_both_spinner_frames_render_identically(self):
        a = W.label(kitty("◐ topic"), HOME + "/x", DARK)
        b = W.label(kitty("◑ topic"), HOME + "/x", DARK)
        self.assertEqual(a, b)

    def test_claude_waiting_accent_robot(self):
        out = W.label(kitty("✳ topic"), HOME + "/x", DARK)
        self.assertTrue(out.startswith(f'<span foreground="{DARK["accent"]}">{ROBOT}</span> topic'), out)

    def test_cwd_omitted_when_topic_already_names_it(self):
        out = W.label(kitty("◐ dotfiles waybar"), HOME + "/dotfiles", DARK)
        self.assertEqual(out, f"{ROBOT} dotfiles waybar")

    def test_no_cwd_no_dim_part(self):
        out = W.label(kitty("◐ topic"), None, DARK)
        self.assertEqual(out, f"{ROBOT} topic")

    def test_remote_shell_host_and_path(self):
        out = W.label(kitty("dsociative@dsdc: ~"), HOME, DARK)
        self.assertEqual(out, f"{SSH} dsdc: ~")

    def test_ssh_command_while_connecting(self):
        out = W.label(kitty("ssh dsdc"), HOME, DARK)
        self.assertEqual(out, f"{SSH} dsdc")

    def test_prompt_is_dim_terminal_with_cwd_title(self):
        for title in ("~", "~/dotfiles", "/etc", "…/a/b/c"):
            with self.subTest(title=title):
                out = W.label(kitty(title), HOME, DARK)
                self.assertEqual(out, f"{DIM_OPEN}{TERM} {title}</span>")

    def test_long_prompt_title_is_clipped_too(self):
        out = W.label(kitty("…/study/kaiten-cli/src"), HOME, DARK)
        self.assertEqual(out, f"{DIM_OPEN}{TERM} …/study/kaiten-cl…</span>")

    def test_running_command_cog_and_dim_cwd(self):
        out = W.label(kitty("cargo build"), HOME + "/proj", DARK)
        self.assertEqual(out, f"{COG} cargo build {DIM_OPEN}proj</span>")

    def test_markup_in_title_is_escaped(self):
        out = W.label(kitty('a & b <i>x</i> "q"'), None, DARK)
        self.assertIn('a &amp; b &lt;i&gt;x&lt;/i&gt; "q"', out)
        self.assertNotIn("<i>", out)

    def test_control_characters_removed(self):
        out = W.label(kitty("a\nb\tc"), None, DARK)
        self.assertNotIn("\n", out)
        self.assertNotIn("\t", out)

    def test_topic_is_clipped(self):
        out = W.label(kitty("◐ " + "x" * 40), None, DARK)
        self.assertEqual(out, f"{ROBOT} " + "x" * 17 + "…")


class OtherClassLabelTest(unittest.TestCase):
    def test_firefox_glyph_and_suffix_stripped(self):
        c = kitty("Some page — Mozilla Firefox", **{"class": "firefox"})
        self.assertEqual(W.label(c, None, DARK), f"{W.GLYPHS['firefox']} Some page")

    def test_long_title_clipped(self):
        c = kitty("(1) stanislav.smirnov - Flant Флант — Mozilla Firefox", **{"class": "firefox"})
        out = W.label(c, None, DARK)
        self.assertEqual(out, f"{W.GLYPHS['firefox']} (1) stanislav.smi…")

    def test_steam_app_uses_steam_glyph(self):
        c = kitty("Game", **{"class": "steam_app_1234"})
        self.assertTrue(W.label(c, None, DARK).startswith(W.GLYPHS["steam"]))

    def test_unknown_class_uses_default_glyph(self):
        c = kitty("Thing", **{"class": "whatever"})
        self.assertEqual(W.label(c, None, DARK), f"{W.GLYPHS['default']} Thing")


MONITORS = [
    {"id": 0, "name": "DP-1", "focused": True, "activeWorkspace": {"id": 4, "name": "4"}},
    {"id": 1, "name": "DP-2", "focused": False, "activeWorkspace": {"id": 3, "name": "3"}},
]
WORKSPACES = [
    {"id": 4, "name": "4", "monitor": "DP-1", "windows": 1},
    {"id": 1, "name": "1", "monitor": "DP-2", "windows": 2},
    {"id": 2, "name": "2", "monitor": "DP-2", "windows": 0},
    {"id": 10, "name": "10", "monitor": "DP-1", "windows": 1},
    {"id": 3, "name": "3", "monitor": "DP-2", "windows": 2},
    {"id": -99, "name": "special:magic", "monitor": "DP-1", "windows": 1},
]
CLIENTS = [
    kitty("◐ right-one", address="0xa", workspace={"id": 1, "name": "1"}, pid=11, at=[1283, 26]),
    kitty("◐ left-one", address="0xb", workspace={"id": 1, "name": "1"}, pid=12, at=[2, 26]),
    kitty("a & b", address="0xc", workspace={"id": 3, "name": "3"}, pid=13, at=[2, 26], focusHistoryID=0),
    kitty("~", address="0xd", workspace={"id": 10, "name": "10"}, pid=14, at=[2561, 25]),
    kitty("Avito page — Mozilla Firefox", **{"class": "firefox"}, address="0xe",
          workspace={"id": 4, "name": "4"}, pid=15, at=[2562, 26]),
    kitty("scratch", address="0xf", workspace={"id": -99, "name": "special:magic"}, pid=16, at=[0, 0]),
    kitty("unmapped", address="0x10", workspace={"id": 4, "name": "4"}, pid=17, at=[0, 0], mapped=False),
    kitty("", address="0x11", workspace={"id": 4, "name": "4"}, pid=18, at=[0, 0], **{"class": ""}),
]
CWDS = {11: HOME + "/study", 12: HOME + "/mind", 13: HOME + "/dotfiles", 14: HOME}


def render(out):
    return W.render(MONITORS, WORKSPACES, CLIENTS, out, DARK, cwd_of=CWDS.get)


class RenderTest(unittest.TestCase):
    def test_only_own_monitor_workspaces(self):
        text = render("DP-2")["text"]
        for present in ("left-one", "right-one", "a &amp; b"):
            self.assertIn(present, text)
        for absent in ("Avito", TERM + " ~", "scratch", "unmapped"):
            self.assertNotIn(absent, text)

    def test_windows_ordered_left_to_right_by_position(self):
        text = render("DP-2")["text"]
        self.assertLess(text.index("left-one"), text.index("right-one"))

    def test_empty_persistent_workspace_shows_just_its_pill(self):
        text = render("DP-2")["text"]
        self.assertIn(f"   {W.pill('2', DARK, active=False)}   ", text)

    def test_every_workspace_number_is_a_bold_pill(self):
        text = render("DP-2")["text"]
        for n in ("1", "2", "3"):
            self.assertIn(f"<b> {n} </b></span>", text)

    def test_active_workspace_pill_is_inverted(self):
        text = render("DP-2")["text"]
        self.assertIn(f'<span background="{DARK["active_bg"]}" foreground="{DARK["active_fg"]}"><b> 3 </b></span>', text)
        self.assertIn(f'<span background="{DARK["ws_bg"]}" foreground="{DARK["ws_fg"]}"><b> 1 </b></span>', text)

    def test_light_palette_has_distinct_active_and_inactive_pills(self):
        for name, pal in W.PALETTES.items():
            with self.subTest(theme=name):
                self.assertNotEqual(pal["active_bg"], pal["ws_bg"])

    def test_focused_window_bold_only_on_its_monitor(self):
        self.assertIn(f"<b>{COG} a &amp; b", render("DP-2")["text"])
        dp1 = render("DP-1")["text"]
        self.assertEqual(dp1.count("<b>"), 2, "only the two workspace pills may be bold on DP-1")

    def test_special_unmapped_and_ghost_windows_skipped(self):
        text = render("DP-1")["text"]
        self.assertNotIn("scratch", text)
        self.assertNotIn("unmapped", text)
        self.assertNotIn("special", text)
        self.assertNotIn(W.GLYPHS["default"], text)

    def test_tooltip_has_full_escaped_title_and_cwd(self):
        tip = render("DP-2")["tooltip"]
        self.assertIn("a &amp; b", tip)
        self.assertIn(HOME + "/dotfiles", tip)

    def test_text_is_single_line(self):
        self.assertNotIn("\n", render("DP-2")["text"])

    def test_no_output_filter_shows_all_monitors(self):
        text = render(None)["text"]
        self.assertIn("left-one", text)
        self.assertIn("Avito", text)


class ThemeTest(unittest.TestCase):
    def test_theme_from_style_symlink(self):
        with tempfile.TemporaryDirectory() as d:
            for target, expected in (("gruvbox.css", "dark"), ("flatwhite.css", "light"), ("black.css", "dark")):
                link = os.path.join(d, "style.css")
                if os.path.lexists(link):
                    os.unlink(link)
                os.symlink(target, link)
                self.assertEqual(W.theme_name(link), expected, target)

    def test_missing_symlink_is_dark(self):
        self.assertEqual(W.theme_name("/nonexistent/style.css"), "dark")


class ShellCwdTest(unittest.TestCase):
    def test_unknown_pid_is_none(self):
        self.assertIsNone(W.shell_cwd(2**22 - 1))

    def test_single_shell_child_cwd(self):
        d = os.path.realpath(tempfile.gettempdir())
        p = subprocess.Popen(["bash", "-c", "sleep 3; true"], cwd=d)
        try:
            time.sleep(0.2)
            self.assertEqual(W.shell_cwd(os.getpid()), d)
        finally:
            p.kill()
            p.wait()

    def test_two_shell_children_is_ambiguous(self):
        procs = [subprocess.Popen(["bash", "-c", "sleep 3; true"]) for _ in range(2)]
        try:
            time.sleep(0.2)
            self.assertIsNone(W.shell_cwd(os.getpid()))
        finally:
            for p in procs:
                p.kill()
                p.wait()


if __name__ == "__main__":
    unittest.main()
