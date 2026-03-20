// Cross-platform opencode notification plugin.
//
// Notification delivery (layered, all channels fire in parallel):
//   1. tmux display-message  -- visible in the status bar for 5s
//   2. tmux bell             -- marks the window; terminal bounces/badges
//   3. OSC 9 escape sequence -- triggers native notification in supporting
//                               terminals (Ghostty, iTerm2, etc.)
//   4. osascript fallback    -- macOS-only, used when tmux is not available
//
// No npm dependencies. Uses only the Bun shell ($) provided by opencode's
// plugin API and process.stdout for escape sequences.

const EVENTS: Record<string, string> = {
  "session.idle": "Session completed",
  "session.error": "Session error",
  "permission.asked": "Permission requested",
}

/** Derive a short project label from the working directory. */
function projectLabel(directory: string): string {
  const parts = directory.replace(/\/+$/, "").split("/")
  return parts[parts.length - 1] || "opencode"
}

/** Send a tmux display-message and bell. */
async function notifyTmux(
  $: any,
  message: string,
): Promise<void> {
  // display-message shows in the status bar; -d is duration in ms
  await $`tmux display-message -d 5000 ${message}`.quiet().nothrow()
  // Bell character marks the window with activity in tmux and causes
  // the outer terminal emulator to bounce/badge.
  process.stdout.write("\x07")
}

/** Send an OSC 9 notification escape sequence.
 *  Supported by Ghostty (macOS Notification Center), iTerm2, foot, etc.
 *  Terminals that don't understand it silently ignore the sequence. */
function notifyOSC9(message: string): void {
  process.stdout.write(`\x1b]9;${message}\x07`)
}

/** Send a native macOS notification via osascript. */
async function notifyOsascript(
  $: any,
  title: string,
  body: string,
): Promise<void> {
  const escaped = body.replace(/"/g, '\\"')
  await $`osascript -e ${'display notification "' + escaped + '" with title "' + title + '"'}`.quiet().nothrow()
}

export const NotificationPlugin = async ({
  project,
  client,
  $,
  directory,
  worktree,
}: {
  project: any
  client: any
  $: any
  directory: string
  worktree: string
}) => {
  const label = projectLabel(directory)
  const inTmux = !!process.env.TMUX
  const isDarwin = process.platform === "darwin"

  return {
    event: async ({ event }: { event: { type: string } }) => {
      const description = EVENTS[event.type]
      if (!description) return

      const message = `opencode [${label}]: ${description}`

      if (inTmux) {
        // tmux display-message + bell (works on macOS and Linux, including
        // over SSH/et since the bell propagates to the local terminal)
        await notifyTmux($, message)
      }

      // OSC 9 -- request a native notification from the terminal emulator.
      // Works over SSH/et because the escape sequence travels through the
      // terminal pipe to the local emulator.
      notifyOSC9(message)

      // Fallback: if we're on macOS without tmux, use osascript directly
      if (isDarwin && !inTmux) {
        await notifyOsascript($, "opencode", `[${label}] ${description}`)
      }
    },
  }
}
