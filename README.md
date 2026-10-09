# Patchwork 3D

**A browser-based, two-player 3D adaptation of the quilt-building strategy game Patchwork.**

Arrange patches on your quilt board, manage buttons and time, and compete to finish with the highest score. The project combines a Three.js 3D interface with OCaml-based game components and Firebase-backed multiplayer synchronization.

### [▶ Play Patchwork 3D](https://ds4015.github.io/oxcaml/patchwork_3d.html)

[Play in browser](https://ds4015.github.io/oxcaml/patchwork_3d.html) · [View source](https://github.com/ds4015/oxcaml) · [Developer portfolio](https://www.dallas-scott.org)

## Features

- **Interactive 3D game board** with quilt boards, patches, tokens, and in-scene UI.
- **Two-player match interface**: host or join using a shared Match ID.
- **Patch placement and time-track mechanics** inspired by the tabletop game.
- **Button economy**: balance the cost, time, and recurring income of patches.
- **Multiplayer state synchronization** implemented with Firebase Authentication and Firestore.
- **Runs in a modern desktop browser** without installing the game locally.

## How to play

1. Open **[Patchwork 3D](https://ds4015.github.io/oxcaml/patchwork_3d.html)**.
2. Enter your player name and a Match ID.
3. Select **Host** to create a match, or **Join** to enter an existing match using the same ID.
4. Take turns choosing between advancing on the time track and selecting an available patch to place on your quilt board.
5. Use buttons to acquire patches, and collect income when you pass income markers on the time track.
6. When both players reach the end of the time track, the game scores your remaining buttons and empty quilt spaces.

**Scoring:** final score = buttons remaining − 2 × empty quilt-board spaces.

> The game requires an active internet connection for its hosted libraries and multiplayer services. Match availability depends on the configured Firebase backend.

## Technology

| Layer | Tools |
| --- | --- |
| 3D interface and interaction | JavaScript, [Three.js](https://threejs.org/) |
| In-scene text | [Troika Three Text](https://github.com/protectwise/troika) |
| Game logic and UI components | OCaml, Bonsai, compiled JavaScript |
| Multiplayer and match persistence | Firebase Authentication, Cloud Firestore |
| Web hosting | GitHub Pages |

## Project structure

| Path | Purpose |
| --- | --- |
| [`patchwork_3d.html`](patchwork_3d.html) | Main browser game page and Host/Join interface |
| [`patchwork_three.js`](patchwork_three.js) | 3D scenes, game board, rendering, and interactions |
| [`multiplayer.js`](multiplayer.js) | Firebase initialization, matchmaking, and multiplayer state synchronization |
| [`logic/`](logic/) and [`ui/`](ui/) | OCaml game and UI source files |
| [`generated_js/`](generated_js/) | Compiled browser-side JavaScript |
| [`img3d/`](img3d/) and [`sounds/`](sounds/) | Visual and audio assets |

## Run locally

For the browser version, clone the repository and serve the files over HTTP:

```bash
git clone --branch gh-pages https://github.com/ds4015/oxcaml.git
cd oxcaml
python3 -m http.server 8000
```

Then visit **http://localhost:8000/patchwork_3d.html**.

Running a local server avoids some browser restrictions on modules and asset loading. The deployed multiplayer service still depends on the Firebase project configuration and access rules. Rebuilding the OCaml-generated files is a separate development step, not necessary for simply viewing the existing web build.

## Development notes

The game page loads `patchwork_three.js`, `multiplayer.js`, and the compiled `generated_js/component.bc.js` bundle. The 3D layer uses Three.js; the multiplayer layer uses Firebase for match state. The repository also includes OCaml development scaffolding inherited from its upstream playground.

## Credits and license

Patchwork 3D is an **unofficial digital project inspired by the tabletop game Patchwork**. This repository is a fork of [yoav-zibin/oxcaml](https://github.com/yoav-zibin/oxcaml); upstream starter code and tooling should be credited separately from the Patchwork-specific work in this fork.

Project maintained by **[Dallas Scott](https://www.dallas-scott.org)**.

See the repository's [MIT license](LICENSE) for the software license. Third-party libraries, artwork, and the underlying tabletop game may carry separate rights.
