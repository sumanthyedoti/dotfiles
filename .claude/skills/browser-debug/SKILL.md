---
name: browser-debug
description: Browser tool selection guide. Prioritize Playwriter over Chrome DevTools.
---

# Browser Tool Selection

## Priority: Playwriter > Chrome DevTools

**Default**: Use Playwriter MCP (user's Chrome via extension)
**Fallback**: Chrome DevTools (headless, when Playwriter unavailable)

## When to Use Each

### Playwriter (Priority 1)

✅ Interactive debugging
✅ Need user's sessions/cookies
✅ Browser extensions required
✅ Context efficiency critical (5-20KB vs 50-100KB)
✅ Development/testing authenticated flows

**Prerequisites**: User enabled Playwriter extension (icon green on target tab)

### Chrome DevTools (Fallback)

⚠️ Playwriter extension not installed/enabled
⚠️ CI/CD clean-slate testing
⚠️ Reproducible test environment needed
⚠️ User explicitly requests headless

**Prerequisites**: Launch Chrome: `chromium --remote-debugging-port=9222 --headless &`

## Key Differences

| Feature    | Playwriter       | Chrome DevTools      |
| ---------- | ---------------- | -------------------- |
| Browser    | User's Chrome    | Headless             |
| Context    | 5-20KB snapshots | 50-100KB screenshots |
| Sessions   | ✅ Preserved     | ❌ Fresh each time   |
| Extensions | ✅ Work          | ❌ None              |
| State      | ✅ Persists      | ❌ New instance      |

## Decision Flow

```
1. Is Playwriter extension enabled (green icon)?
   YES → Use Playwriter
   NO → Ask user to enable, or use Chrome DevTools

2. Need authenticated sessions?
   YES → Must use Playwriter

3. Need browser extensions?
   YES → Must use Playwriter

4. Default → Use Playwriter (80% less context)
```

## Troubleshooting

**Playwriter "Extension not running"**
→ User clicks Playwriter icon on tab (turns green when connected)

**Chrome DevTools not responding**
→ Check Chrome running: `ps aux | grep chromium`
→ Relaunch: `chromium --remote-debugging-port=9222 --headless &`
