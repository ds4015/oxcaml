/* Dallas Scott - ds4015
   Patchwork Three.JS UI elements
*/

import * as THREE from "three";
import { EffectComposer, OrbitControls } from "three/examples/jsm/Addons.js";
import { MTLLoader } from "three/examples/jsm/loaders/MTLLoader.js";
import { RenderPass } from "three/examples/jsm/postprocessing/RenderPass.js";
import { OutlinePass } from "three/examples/jsm/postprocessing/OutlinePass.js";
import { OBJLoader } from "three/examples/jsm/loaders/OBJLoader.js";

/* setup  */
const scene = new THREE.Scene();
const scene2 = new THREE.Scene();
const camera = new THREE.PerspectiveCamera(
    75,
    window.innerWidth / window.innerHeight,
    0.1,
    1000,
);
const camera2 = new THREE.PerspectiveCamera(
    75,
    window.innerWidth / window.innerHeight,
    0.1,
    1000,
);

const texLoader = new THREE.TextureLoader();

const canvas = document.getElementById("three-js-canvas");
const renderer = new THREE.WebGLRenderer({ canvas: canvas, antialias: true });
renderer.setSize(window.innerWidth, window.innerHeight);
document.body.appendChild(renderer.domElement);
const controls = new OrbitControls(camera, renderer.domElement);
controls.target.set(0, 0, 0);
controls.update();

camera.position.z = 2.3;
camera2.position.z = 5.3;
camera2.position.y = 4;
const cam_offset = -2.3;
scene.add(camera);
scene2.add(camera2);
const p1_buttons = document.getElementById("p1-buttons");
const p2_buttons = document.getElementById("p2-buttons");
const player_name = document.getElementById("player-box-1");
const ai_name = document.getElementById("player-box-2");

const ui_elements = [];
let ui_element_percentages = [];
let clickables = [];
let patch_clickables = [];
let draggable = [];

/* responsive canvas */
window.addEventListener("resize", onWindowResize);

function onWindowResize() {
    const container = document.querySelector(".container");
    const width = container.clientWidth;
    const height = container.clientHeight;

    camera.aspect = width / height;
    camera.updateProjectionMatrix();
    camera2.aspect = width / height;
    camera2.updateProjectionMatrix();

    renderer.setSize(width, height);
    vw_to_local();
}

/* skybox */
let skydome;
let skydome2;
const colorTex = texLoader.load("img3d/-Color.png", (tex) => {
    tex.colorSpace = THREE.SRGBColorSpace;
    tex.magFilter = THREE.LinearFilter;
    tex.minFilter = THREE.LinearFilter;
});
const objLoader = new OBJLoader();
objLoader.load("img3d/uvsphere_3.obj", (obj) => {
    obj.traverse((child) => {
        if (child.isMesh) {
            child.material = new THREE.MeshBasicMaterial({
                map: colorTex,
                side: THREE.BackSide,
            });
        }
    });

    obj.scale.set(300, 300, 300);
    skydome = obj;
    skydome2 = obj.clone(true);
    scene.add(obj);
    scene2.add(skydome2);
});

/* UI button currency icons */
const b1 = make_button(0.05, 0.15);
b1.rotation.y += 0.18;
b1.rotation.z += 0.09;
const b2 = make_button(0.9, 0.15);
b2.rotation.y -= 0.18;
b2.rotation.z -= 0.09;

function make_button(x_pct, y_pct) {
    const b_geom = new THREE.TorusGeometry(0.07, 0.01, 8, 24);
    const c_geom = new THREE.CircleGeometry(0.08, 32);
    const h_geom = new THREE.CircleGeometry(0.01, 32);
    const b_mat = new THREE.MeshToonMaterial({
        color: 0x4169e1,
        side: THREE.DoubleSide,
    });
    const h_mat = new THREE.MeshBasicMaterial({ color: 0xffffff });
    const button_mesh = new THREE.Mesh(b_geom, b_mat);
    const button_interior = new THREE.Mesh(c_geom, b_mat);
    const button = new THREE.Group();
    button_interior.position.z = 0;
    const h_mesh = new THREE.Mesh(h_geom, h_mat);
    const h_mesh2 = new THREE.Mesh(h_geom, h_mat);
    const h_mesh3 = new THREE.Mesh(h_geom, h_mat);
    const h_mesh4 = new THREE.Mesh(h_geom, h_mat);

    h_mesh.position.x = -0.015;
    h_mesh.position.y = 0.015;
    h_mesh.position.z = 0.012;
    h_mesh2.position.x = 0.02;
    h_mesh2.position.y = 0.015;
    h_mesh2.position.z = 0.012;
    h_mesh3.position.x = -0.015;
    h_mesh3.position.y = -0.015;
    h_mesh3.position.z = 0.012;
    h_mesh4.position.x = 0.02;
    h_mesh4.position.y = -0.015;
    h_mesh4.position.z = 0.012;
    const button_edges = new THREE.EdgesGeometry(b_geom);
    const button_outline = new THREE.LineSegments(
        button_edges,
        new THREE.LineBasicMaterial({ color: 0x000000 }),
    );
    button.add(h_mesh);
    button.add(h_mesh2);
    button.add(h_mesh3);
    button.add(h_mesh4);
    button.add(button_mesh);
    button.add(button_interior);

    ui_elements.push(button);
    ui_element_percentages.push(x_pct);
    ui_element_percentages.push(y_pct);
    camera.add(button);
    vw_to_local();
    return button;
}

/* main board */
const geometry = new THREE.BoxGeometry(1.1, 1.1, 0.01);
const material = new THREE.MeshBasicMaterial({ color: 0x00ff00 });
const texture = texLoader.load("img3d/bg.png");
texture.colorSpace = THREE.SRGBColorSpace;
const mat = new THREE.MeshBasicMaterial({ map: texture });
const cube = new THREE.Mesh(geometry, mat);

const main_board_cells = [];
const single_patches = [];

const board = new THREE.Group();
let board2 = new THREE.Group();
create_grid_cells();
scene.add(board);

const table_geom = new THREE.CylinderGeometry(1.25, 1.25, 0.01, 50);
const table_g = new THREE.EdgesGeometry(table_geom);
const table_m = new THREE.MeshStandardMaterial({
    color: 0xffffff,
    roughness: 0,
    metalness: 0,
    transparent: true,
    opacity: 0.05,
});
const table = new THREE.Mesh(table_geom, table_m);
table.position.y = -1.2;
const table_mat = new THREE.LineBasicMaterial({
    color: 0xffffff,
    linewidth: 1,
});
const table_edges = new THREE.LineSegments(table_g, table_mat);
table_edges.renderOrder = 1;
table_mat.depthTest = false;

table.add(table_edges);
scene.add(table);

function create_grid_cells() {
    const board_button = new THREE.CylinderGeometry(0.03, 0.03, 0.015, 40);

    const single = new THREE.BoxGeometry(0.12, 0.12, 0.012);
    const double = new THREE.BoxGeometry(0.185, 0.12, 0.012);
    const triple = new THREE.BoxGeometry(0.12, 0.38, 0.012);
    const end_zone = new THREE.BoxGeometry(0.247, 0.247, 0.012);
    const single_patch = new THREE.BoxGeometry(0.08, 0.08, 0.03);

    const m = new THREE.MeshBasicMaterial({ color: 0xababab });
    const b = new THREE.MeshBasicMaterial({ color: 0x0000ff });
    function full_singles_row(x, r) {
        for (let i = 0; i < x; i++) {
            const mesh = new THREE.Mesh(single, m);
            mesh.position.set(-0.455 + i * 0.13, 0.455 - 0.13 * r, 0);
            board.add(mesh);
            main_board_cells.push(mesh);
        }
    }

    // rows 1, 3
    full_singles_row(8, 0);
    full_singles_row(8, 2);

    // row 2
    const r2c1 = new THREE.Mesh(single, m);
    r2c1.position.set(-0.455 + 0 * 0.13, 0.455 - 0.13 * 1);
    const r2c2 = new THREE.Mesh(single, m);
    r2c2.position.set(-0.455 + 1 * 0.13, 0.455 - 0.13 * 1);
    const r2c3to3p5 = new THREE.Mesh(double, m);
    r2c3to3p5.position.set(-0.455 + 2.25 * 0.13, 0.455 - 0.13 * 1);
    const r2c3p5to5 = new THREE.Mesh(double, m);
    r2c3p5to5.position.set(-0.455 + 3.75 * 0.13, 0.455 - 0.13 * 1);
    const r2c6 = new THREE.Mesh(single, m);
    r2c6.position.set(-0.455 + 5 * 0.13, 0.455 - 0.13 * 1);
    const r2c7 = new THREE.Mesh(single, m);
    r2c7.position.set(-0.455 + 6 * 0.13, 0.455 - 0.13 * 1);
    const r2c8 = new THREE.Mesh(single, m);
    r2c8.position.set(-0.455 + 7 * 0.13, 0.455 - 0.13 * 1);
    board.add(r2c1);
    board.add(r2c2);
    board.add(r2c3to3p5);
    board.add(r2c3p5to5);
    board.add(r2c6);
    board.add(r2c7);
    board.add(r2c8);
    main_board_cells.push(r2c1);
    main_board_cells.push(r2c2);
    main_board_cells.push(r2c3p5to5);
    main_board_cells.push(r2c3p5to5);
    main_board_cells.push(r2c6);
    main_board_cells.push(r2c7);
    main_board_cells.push(r2c8);

    // row 4
    const r3c1 = new THREE.Mesh(single, m);
    r3c1.position.set(-0.455 + 0 * 0.13, 0.455 - 0.13 * 3);
    const r3c2 = new THREE.Mesh(single, m);
    r3c2.position.set(-0.455 + 1 * 0.13, 0.455 - 0.13 * 3);
    const r3c3 = new THREE.Mesh(double, m);
    r3c3.position.set(-0.455 + 2 * 0.13, 0.455 - 0.13 * 3.25);
    r3c3.rotation.z = 1.57079;
    const r3c4 = new THREE.Mesh(end_zone, m);
    r3c4.position.set(-0.455 + 3.49 * 0.13, 0.455 - 0.13 * 3.49);
    const r3c6 = new THREE.Mesh(double, m);
    r3c6.position.set(-0.455 + 5 * 0.13, 0.455 - 0.13 * 3.25);
    r3c6.rotation.z = 1.57079;
    const r3c7 = new THREE.Mesh(single, m);
    r3c7.position.set(-0.455 + 6 * 0.13, 0.455 - 0.13 * 3);
    const r3c8 = new THREE.Mesh(single, m);
    r3c8.position.set(-0.455 + 7 * 0.13, 0.455 - 0.13 * 3);

    board.add(r3c1);
    board.add(r3c2);
    board.add(r3c3);
    board.add(r3c4);
    board.add(r3c6);
    board.add(r3c7);
    board.add(r3c8);
    main_board_cells.push(r3c1);
    main_board_cells.push(r3c2);
    main_board_cells.push(r3c3);
    main_board_cells.push(r3c4);
    main_board_cells.push(r3c6);
    main_board_cells.push(r3c7);
    main_board_cells.push(r3c8);

    // row 5
    const r5c1 = new THREE.Mesh(single, m);
    r5c1.position.set(-0.455 + 0 * 0.13, 0.455 - 0.13 * 4);
    const r5c2 = new THREE.Mesh(single, m);
    r5c2.position.set(-0.455 + 1 * 0.13, 0.455 - 0.13 * 4);
    const r5c3 = new THREE.Mesh(double, m);
    r5c3.position.set(-0.455 + 2 * 0.13, 0.455 - 0.13 * 4.75);
    r5c3.rotation.z = 1.57079;
    const r5c6 = new THREE.Mesh(double, m);
    r5c6.position.set(-0.455 + 5 * 0.13, 0.455 - 0.13 * 4.75);
    r5c6.rotation.z = 1.57079;
    const r5c7 = new THREE.Mesh(double, m);
    r5c7.rotation.z = 1.57079;
    r5c7.position.set(-0.455 + 6 * 0.13, 0.455 - 0.13 * 4.25);
    const r5c8 = new THREE.Mesh(single, m);
    r5c8.position.set(-0.455 + 7 * 0.13, 0.455 - 0.13 * 4);

    board.add(r5c1);
    board.add(r5c2);
    board.add(r5c3);
    board.add(r5c6);
    board.add(r5c7);
    board.add(r5c8);
    main_board_cells.push(r5c1);
    main_board_cells.push(r5c2);
    main_board_cells.push(r5c3);
    main_board_cells.push(r5c6);
    main_board_cells.push(r5c7);
    main_board_cells.push(r5c8);

    //row 6
    const r6c1 = new THREE.Mesh(triple, m);
    r6c1.position.set(-0.455 + 0 * 0.13, 0.455 - 0.13 * 6);
    const r6c2 = new THREE.Mesh(double, m);
    r6c2.rotation.z = 1.57079;
    r6c2.position.set(-0.455 + 1 * 0.13, 0.455 - 0.13 * 5.25);
    const r6c4 = new THREE.Mesh(single, m);
    r6c4.position.set(-0.455 + 3 * 0.13, 0.455 - 0.13 * 5);
    const r6c5 = new THREE.Mesh(single, m);
    r6c5.position.set(-0.455 + 4 * 0.13, 0.455 - 0.13 * 5);
    const r6c7 = new THREE.Mesh(double, m);
    r6c7.rotation.z = 1.57079;
    r6c7.position.set(-0.455 + 6 * 0.13, 0.455 - 0.13 * 5.75);
    const r6c8 = new THREE.Mesh(single, m);
    r6c8.position.set(-0.455 + 7 * 0.13, 0.455 - 0.13 * 5);

    board.add(r6c1);
    board.add(r6c2);
    board.add(r6c4);
    board.add(r6c5);
    board.add(r6c7);
    board.add(r6c8);
    main_board_cells.push(r6c1);
    main_board_cells.push(r6c2);
    main_board_cells.push(r6c4);
    main_board_cells.push(r6c5);
    main_board_cells.push(r6c7);
    main_board_cells.push(r6c8);

    // row 7
    const r7c2 = new THREE.Mesh(double, m);
    r7c2.rotation.z = 1.57079;
    r7c2.position.set(-0.455 + 1 * 0.13, 0.455 - 0.13 * 6.75);
    const r7c3 = new THREE.Mesh(single, m);
    r7c3.position.set(-0.455 + 2 * 0.13, 0.455 - 0.13 * 6);
    const r7c4 = new THREE.Mesh(single, m);
    r7c4.position.set(-0.455 + 3 * 0.13, 0.455 - 0.13 * 6);
    const r7c5 = new THREE.Mesh(single, m);
    r7c5.position.set(-0.455 + 4 * 0.13, 0.455 - 0.13 * 6);
    const r7c6 = new THREE.Mesh(single, m);
    r7c6.position.set(-0.455 + 5 * 0.13, 0.455 - 0.13 * 6);
    const r7c8 = new THREE.Mesh(single, m);
    r7c8.position.set(-0.455 + 7 * 0.13, 0.455 - 0.13 * 6);

    board.add(r7c2);
    board.add(r7c3);
    board.add(r7c4);
    board.add(r7c5);
    board.add(r7c6);
    board.add(r7c8);

    main_board_cells.push(r7c2);
    main_board_cells.push(r7c3);
    main_board_cells.push(r7c4);
    main_board_cells.push(r7c5);
    main_board_cells.push(r7c6);
    main_board_cells.push(r7c8);

    //row 8
    const r8c3 = new THREE.Mesh(single, m);
    r8c3.position.set(-0.455 + 2 * 0.13, 0.455 - 0.13 * 7);
    const r8c4 = new THREE.Mesh(single, m);
    r8c4.position.set(-0.455 + 3 * 0.13, 0.455 - 0.13 * 7);
    const r8c5 = new THREE.Mesh(single, m);
    r8c5.position.set(-0.455 + 4 * 0.13, 0.455 - 0.13 * 7);
    const r8c6 = new THREE.Mesh(single, m);
    r8c6.position.set(-0.455 + 5 * 0.13, 0.455 - 0.13 * 7);
    const r8c7 = new THREE.Mesh(single, m);
    r8c7.position.set(-0.455 + 6 * 0.13, 0.455 - 0.13 * 7);
    const r8c8 = new THREE.Mesh(single, m);
    r8c8.position.set(-0.455 + 7 * 0.13, 0.455 - 0.13 * 7);

    board.add(r8c3);
    board.add(r8c4);
    board.add(r8c5);
    board.add(r8c6);
    board.add(r8c7);
    board.add(r8c8);

    main_board_cells.push(r8c3);
    main_board_cells.push(r8c4);
    main_board_cells.push(r8c5);
    main_board_cells.push(r8c6);
    main_board_cells.push(r8c7);
    main_board_cells.push(r8c8);

    // special patches
    const sp_pat_texture = texLoader.load("img3d/leather4.png", (tex) => {
        tex.colorSpace = THREE.SRGBColorSpace;
    });
    const sp_pat_mat = new THREE.MeshBasicMaterial({ map: sp_pat_texture });
    const sp1 = new THREE.Mesh(single_patch, sp_pat_mat);
    sp1.material.color.set(0xcccccc);
    sp1.position.set(-0.455 + 3 * 0.13, 0.455 - 0.13 * 1);
    sp1.rotation.z = -0.11;
    const sp2 = new THREE.Mesh(single_patch, sp_pat_mat);
    sp2.position.set(-0.455 + 2 * 0.13, 0.455 - 0.13 * 4);
    sp2.rotation.z = 0.05;
    const sp3 = new THREE.Mesh(single_patch, sp_pat_mat);
    sp3.position.set(-0.455 + 5 * 0.13, 0.455 - 0.13 * 4);
    sp3.rotation.z = -0.17;
    const sp4 = new THREE.Mesh(single_patch, sp_pat_mat);
    sp4.position.set(-0.455 + 6 * 0.13, 0.455 - 0.13 * 5);
    sp4.rotation.z = 0.1;
    const sp5 = new THREE.Mesh(single_patch, sp_pat_mat);
    sp5.position.set(-0.455 + 1 * 0.13, 0.455 - 0.13 * 6);
    sp5.rotation.z = -0.16;
    board.add(sp1);
    board.add(sp2);
    board.add(sp3);
    board.add(sp4);
    board.add(sp5);
    single_patches.push(sp1);
    single_patches.push(sp2);
    single_patches.push(sp3);
    single_patches.push(sp4);
    single_patches.push(sp5);

    // button income
    const b1 = new THREE.Mesh(board_button, b);
    b1.position.set(-0.465, 0.455 - 0.13 * 0.5);
    b1.rotation.set(-1.57079, 0, 0);
    const b2 = new THREE.Mesh(board_button, b);
    b2.position.set(-0.455 + 5.5 * 0.13, 0.455 - 0.13 + 0.23 * 0.5);
    b2.rotation.set(-1.57079, 0, 0);
    const b3 = new THREE.Mesh(board_button, b);
    b3.position.set(-0.465 + 1 * 0.13, 0.455 - 0.13 * 2.5);
    b3.rotation.set(-1.57079, 0, 0);
    const b4 = new THREE.Mesh(board_button, b);
    b4.position.set(-0.455 + 3.5 * 0.13, 0.455 - 0.13 * 2 + 0.01);
    b4.rotation.set(-1.57079, 0, 0);
    const b5 = new THREE.Mesh(board_button, b);
    b5.position.set(-0.455 + 6 * 0.13 + 0.01, 0.455 - 0.13 * 1.5);
    b5.rotation.set(-1.57079, 0, 0);
    const b6 = new THREE.Mesh(board_button, b);
    b6.position.set(-0.455 + 3 * 0.13 + 0.01, 0.455 - 0.13 * 4.5);
    b6.rotation.set(-1.57079, 0, 0);
    const b7 = new THREE.Mesh(board_button, b);
    b7.position.set(-0.455 + 7 * 0.13 + 0.01, 0.455 - 0.13 * 4.5);
    b7.rotation.set(-1.57079, 0, 0);
    const b8 = new THREE.Mesh(board_button, b);
    b8.position.set(-0.455 + 3.5 * 0.13, 0.455 - 0.13 * 6);
    b8.rotation.set(-1.57079, 0, 0);
    const b9 = new THREE.Mesh(board_button, b);
    b9.position.set(-0.455 + 3.5 * 0.13, 0.455 - 0.13 * 7);
    b9.rotation.set(-1.57079, 0, 0);

    board.add(b1);
    board.add(b2);
    board.add(b3);
    board.add(b4);
    board.add(b5);
    board.add(b6);
    board.add(b7);
    board.add(b8);
    board.add(b9);

    function gen_thread(l, o, f) {
        const length = l;
        const amp = 0.002;
        const freq = f;
        const pts = [];
        const steps = 64;
        for (let i = 0; i <= steps; i++) {
            const t = i / steps;
            if (o == "h") {
                const x = t * length;
                const y = Math.sin(t * Math.PI * freq) * amp;
                pts.push(new THREE.Vector3(x, y, 0));
            } else if (o == "v") {
                const y = t * length;
                const x = Math.sin(t * Math.PI * freq) * amp;
                pts.push(new THREE.Vector3(x, y, 0));
            }
        }
        const path = new THREE.CatmullRomCurve3(pts, false, "centripetal");
        const pipe = new THREE.TubeGeometry(path, 64, 0.01, 24, false);
        const tube = new THREE.Mesh(pipe, mat);

        return tube;
    }

    function gen_spiral() {}

    const loop = new THREE.TorusKnotGeometry(0.01, 0.01, 64, 24, 1, 1);
    const thread1 = gen_thread(1.0, "v", 30);
    thread1.position.set(-0.455 + 0.5 * 0.13, 0.455 - 0.13 * 8.25);
    const thread2 = gen_thread(0.78, "h", 25);
    thread2.position.set(-0.455 + 0.5 * 0.13, 0.455 - 0.13 * 0.5);
    const thread3 = gen_thread(0.78, "v", 25);
    thread3.position.set(-0.455 + 6.5 * 0.13, 0.455 - 0.13 * 6.5);
    const thread4 = gen_thread(0.65, "h", 20);
    thread4.position.set(-0.455 + 1.5 * 0.13, 0.455 - 0.13 * 6.5);
    const thread5 = gen_thread(0.65, "v", 20);
    thread5.position.set(-0.455 + 1.5 * 0.13, 0.455 - 0.13 * 6.5);
    const thread6 = gen_thread(0.52, "h", 15);
    thread6.position.set(-0.455 + 1.5 * 0.13, 0.455 - 0.13 * 1.5);
    const thread7 = gen_thread(0.52, "v", 15);
    thread7.position.set(-0.455 + 5.5 * 0.13, 0.455 - 0.13 * 5.5);
    const thread8 = gen_thread(0.4, "h", 10);
    thread8.position.set(-0.455 + 2.5 * 0.13, 0.455 - 0.13 * 5.5);
    const thread9 = gen_thread(0.39, "v", 10);
    thread9.position.set(-0.455 + 2.5 * 0.13, 0.455 - 0.13 * 5.5);
    const thread10 = gen_thread(0.26, "h", 7);
    thread10.position.set(-0.455 + 2.5 * 0.13, 0.455 - 0.13 * 2.5);

    const thread11 = gen_thread(0.26, "v", 7);
    thread11.position.set(-0.455 + 4.5 * 0.13, 0.455 - 0.13 * 4.5);
    const thread12 = gen_thread(0.14, "h", 5);
    thread12.position.set(-0.455 + 3.5 * 0.13, 0.455 - 0.13 * 4.5);

    const final_thread = gen_thread(0.13, "v", 3);
    final_thread.position.set(-0.455 + 3.5 * 0.13, 0.455 - 0.13 * 4.5);
    const tube_loop = new THREE.Mesh(loop, mat);
    tube_loop.position.set(-0.455 + 0.5 * 0.13, 0.455 - 0.13 * 0.4);

    board.add(thread2);
    board.add(thread1);
    board.add(thread3);
    board.add(thread4);
    board.add(thread5);
    board.add(thread6);
    board.add(thread7);
    board.add(thread8);
    board.add(thread9);
    board.add(thread10);
    board.add(thread11);
    board.add(thread12);
    board.add(final_thread);
    board.add(tube_loop);
}
board.add(cube);
board2 = board.clone(true);
board2.rotation.x = -1.57059;
board2.scale.multiplyScalar(1.3);
scene2.add(board2);

scene.background = new THREE.Color(0xffffff);
const dirLight = new THREE.DirectionalLight(0xffffff, 1.5);
dirLight.position.set(0, 0, 1);
scene.add(dirLight);
scene2.add(dirLight.clone(true));

/* tokens */
const time_token_geometry_1 = new THREE.TetrahedronGeometry(0.035);
const time_token_geometry_2 = new THREE.CylinderGeometry(0.03, 0.03, 0.05, 42);
const p1_tt_mat = new THREE.MeshToonMaterial({ color: 0x32cd32 });
const p2_tt_mat = new THREE.MeshToonMaterial({ color: 0xccff00 });
const p1_tt = new THREE.Mesh(time_token_geometry_1, p1_tt_mat);
const p2_tt = new THREE.Mesh(time_token_geometry_2, p2_tt_mat);
p1_tt.position.set(-0.455, 0.455 - 0.13 * 5, 0.025);
p1_tt.rotation.z = Math.PI / 4;
p1_tt.rotation.x = Math.PI / 3.5;
p2_tt.position.set(0, 0, 0);
p2_tt.rotation.set(-1.57079, 0, 0);
board.add(p1_tt);
board.add(p2_tt);
draggable.push(p1_tt);
const tt1_edges = new THREE.EdgesGeometry(time_token_geometry_1);
const tt1_outline = new THREE.LineSegments(
    tt1_edges,
    new THREE.LineBasicMaterial({ color: 0x000000 }),
);
const tt2_edges = new THREE.EdgesGeometry(time_token_geometry_2);
const tt2_outline = new THREE.LineSegments(
    tt2_edges,
    new THREE.LineBasicMaterial({ color: 0x000000 }),
);
//p1_tt.add(tt1_outline);
p2_tt.add(tt2_outline);

/* patches - test only, dims to be passed in from ocaml */
const Square = [1, "U", 1, "R", 1, "D"];
const SquareNub = [1, "U", 1, "R", 1, "U", 1, "SD", 1, "D"];
const SquareHighFive = [1, "U", 2, "R", 1, "U", 1, "SD", 1, "SL", 1, "D"];
const TCross = [2, "U", 1, "L", 1, "SR", 1, "R", 1, "SL", 2, "U"];
const SPatch = [1, "R", 4, "U", 1, "R"];
const LongI = [4, "U"];
const LHalfH = [1, "U", 1, "SD", 3, "R", 1, "U"];
const SHalfH = [1, "U", 1, "SD", 2, "R", 1, "U"];
const HPatch = [2, "U", 1, "SD", 2, "R", 1, "U", 1, "SD", 1, "D"];
const Corner = [1, "U", 1, "L"];
const CornerRev = [1, "L", 1, "U"];
const SLVert = [2, "U", 1, "L", 1, "U"];
const ShortI = [2, "U"];
const IPatch = [3, "U"];
const LRev = [1, "R", 2, "U"];
const LongL = [1, "L", 3, "U"];
const LPatch = [1, "L", 2, "U"];
const ChunkyLRev = [1, "R", 2, "U", 1, "SD", 1, "L"];
const SmallI = [1, "U"];
const ShortT = [2, "U", 1, "R", 1, "SL", 1, "L"];
const StubbyT = [1, "U", 1, "R", 1, "SL", 1, "L"];
const TPatch = [3, "U", 1, "R", 1, "SL", 1, "L"];
const Plus = [1, "U", 1, "R", 1, "SL", 1, "L", 1, "SR", 1, "U"];
const Zig = [1, "U", 1, "R", 1, "U"];
const ZigZag = [1, "U", 1, "L", 1, "U", 1, "L"];
const ZigRev = [1, "U", 1, "L", 1, "U"];
const ChunkyZig = [2, "U", 1, "R", 1, "U", 1, "SD", 1, "D"];
const Cross = [2, "U", 1, "R", 1, "SL", 1, "L", 1, "SR", 2, "U"];
const INub = [1, "U", 1, "L", 1, "SR", 2, "U"];
const WideStubbyT = [1, "R", 1, "U", 1, "R", 1, "SL", 2, "L"];
const Prong = [1, "U", 1, "R", 1, "U", 1, "SD", 1, "R", 1, "D"];
const Vine = [1, "U", 1, "R", 1, "SL", 1, "U", 1, "L", 1, "SR", 2, "U"];
const WidePlus = [
    1,
    "U",
    1,
    "L",
    1,
    "SR",
    1,
    "U",
    1,
    "R",
    1,
    "D",
    1,
    "R",
    1,
    "SL",
    1,
    "D",
];

const patch_dimensions = [
    Square,
    SquareNub,
    SquareHighFive,
    TCross,
    SPatch,
    LongI,
    LHalfH,
    SHalfH,
    HPatch,
    Corner,
    CornerRev,
    SLVert,
    ShortI,
    LRev,
    LongL,
    LPatch,
    ChunkyLRev,
    SmallI,
    IPatch,
    ShortT,
    StubbyT,
    TPatch,
    Plus,
    Zig,
    ZigZag,
    ZigRev,
    ChunkyZig,
    Cross,
    INub,
    WideStubbyT,
    Prong,
    Vine,
    WidePlus,
];

const patch_cols = [
    2, 2, 3, 3, 3, 1, 4, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 3, 3, 3, 3, 2, 3,
    2, 2, 3, 2, 4, 3, 3, 4,
];

const patch_rows = [
    2, 3, 3, 5, 5, 5, 2, 2, 3, 2, 2, 4, 3, 3, 4, 3, 3, 2, 4, 3, 2, 4, 3, 3, 3,
    3, 4, 5, 4, 2, 3, 5, 3,
];

const patches = new THREE.Group();
create_patches();
scene.add(patches);
let patches2 = patches.clone(true);
scene2.add(patches2);

function create_patches() {
    const patch_cell_geo = new THREE.BoxGeometry(0.06, 0.06, 0.012);

    for (let i = 0; i < patch_dimensions.length; i++) {
        const randomColor = Math.floor(Math.random() * 0xffffff);
        const patch_material = new THREE.MeshBasicMaterial({
            color: randomColor,
        });
        let up_offset = 0;
        let down_offset = 0;
        let left_offset = 0;
        let right_offset = 0;
        const o = 0.06;

        const patch = new THREE.Group();
        const initial_patch_cell = new THREE.Mesh(
            patch_cell_geo,
            patch_material,
        );
        patch.add(initial_patch_cell);
        for (let j = 0; j < patch_dimensions[i].length - 1; j += 2) {
            const num_cubes = patch_dimensions[i][j];
            const direction = patch_dimensions[i][j + 1];
            for (let k = 0; k < num_cubes; k++) {
                const patch_cell = new THREE.Mesh(
                    patch_cell_geo,
                    patch_material,
                );
                switch (direction) {
                    case "U":
                        up_offset -= o;
                        break;
                    case "D":
                        down_offset += o;
                        break;
                    case "L":
                        left_offset -= o;
                        break;
                    case "R":
                        right_offset += o;
                        break;
                    case "SU":
                        up_offset -= o;
                    case "SD":
                        down_offset += o;
                        break;
                    case "SL":
                        left_offset -= o;
                        break;
                    case "SR":
                        right_offset += o;
                        break;
                    default:
                        "Invalid direction";
                }
                if (
                    direction === "SU" ||
                    direction === "SD" ||
                    direction === "SL" ||
                    direction === "SR"
                )
                    continue;
                const x_offset = left_offset + right_offset;
                const y_offset = up_offset + down_offset;
                patch_cell.position.set(x_offset, y_offset, 0);
                patch_clickables.push(patch_cell);
                patch.add(patch_cell);
                patch.position.set(0, 0, 0);
                patch.rotation.z = Math.PI;
                patch.rotation.y = Math.PI;
                patches.add(patch);
            }
        }
    }
    position_patches();
    function position_patches() {
        const gapAngle = 0.062;
        let angleCursor = 0;
        for (let i = 0; i < patches.children.length; i++) {
            const cols = patch_cols[i];
            const patchWidth = cols * 0.06;
            const angularWidth = patchWidth / 1.15;
            const angle = angleCursor + angularWidth / 4;

            const r = i * 0.19;
            const x = Math.sin(angle) * 1.15;
            const y = Math.cos(angle) * 1.15;
            patches.children[i].position.set(x, y, 0.2);
            patches.children[i].rotation.z += angle;
            angleCursor += angularWidth + gapAngle;
        }
    }
}

/* drag tokens */
let dragging = null;
let dragOffset = new THREE.Vector3();
const worldPos = new THREE.Vector3();
let dragZ = 0;
let intersectionPlane = new THREE.Plane();
intersectionPlane.set(new THREE.Vector3(0, 0, 1), 0);
let intersectionPoint = new THREE.Vector3();
const normal = new THREE.Vector3();
normal.copy(camera.position).normalize();

/* outlines + highlights */
let object_outlined = null;
let current_highlight = null;

/* raycasting + listeners */
const raycaster = new THREE.Raycaster();
const mouse = new THREE.Vector2();

renderer.domElement.addEventListener("pointerdown", (event) => {
    const rect = renderer.domElement.getBoundingClientRect();

    mouse.x = ((event.clientX - rect.left) / rect.width) * 2 - 1;
    mouse.y = -((event.clientY - rect.top) / rect.height) * 2 + 1;

    raycaster.setFromCamera(mouse, camera);
    const hits = raycaster.intersectObjects(clickables, true);
    const patch_hits = raycaster.intersectObjects(patch_clickables, true);
    const drag_hits = raycaster.intersectObjects(draggable, true);

    if (object_outlined) {
        scene.remove(object_outlined);
        object_outlined.geometry.dispose();
        object_outlined.material.dispose();
        object_outlined = null;
    }

    /* outlines */
    if (hits.length > 0) {
        const chosen = hits[0].object;
        console.log(chosen);
        object_outlined = outline_object(chosen);
        object_outlined.position.x += chosen.parent.position.x;
        scene.add(object_outlined);
    }

    /* patch select overlay */
    if (patch_hits.length > 0) {
        const chosen = patch_hits[0].object;
        console.log(chosen);
        highlight_patch(chosen.parent);
    }

    /* drag time token */
    if (drag_hits.length > 0) {
        const hit = drag_hits[0];
        dragging = hit.object;
        dragging.scale.multiplyScalar(1.5);
        dragZ = dragging.position.z;
        toggle_orbit_controls("off");
        dragging.getWorldPosition(worldPos);

        dragOffset.copy(hit.point).sub(worldPos);
    }
});

renderer.domElement.addEventListener("pointerup", () => {
    if (dragging != null) dragging.scale.multiplyScalar(1 / 1.5);
    dragging = null;
    toggle_orbit_controls("on");
});

renderer.domElement.addEventListener("pointermove", (event) => {
    if (!dragging) return;

    const rect = renderer.domElement.getBoundingClientRect();

    mouse.x = ((event.clientX - rect.left) / rect.width) * 2 - 1;
    mouse.y = -((event.clientY - rect.top) / rect.height) * 2 + 1;

    raycaster.setFromCamera(mouse, camera);

    if (raycaster.ray.intersectPlane(intersectionPlane, intersectionPoint)) {
        intersectionPoint.sub(dragOffset);
        const parent = dragging.parent;
        if (parent) {
            parent.worldToLocal(intersectionPoint);
            intersectionPoint.z = dragZ;
            dragging.position.copy(intersectionPoint);
        } else {
            intersectionPoint.z = dragZ;
            dragging.position.copy(intersectionPoint);
        }
    }
});

/* quilt boards */
const p1_quilt_board = create_quilt_board(new THREE.Color(0x0000ff));
p1_quilt_board.position.x = -2.2;
scene.add(p1_quilt_board);
const p2_quilt_board = create_quilt_board(new THREE.Color(0xff0000));
p2_quilt_board.position.x = 2.2;
scene.add(p2_quilt_board);
//toggle_element(board, "off");
//toggle_element(patches, "off");
//toggle_element(table, "off");

function create_quilt_board(color) {
    const qb = new THREE.Group();
    const q_board_geo = new THREE.PlaneGeometry(1.15, 1.15);
    const q_board_m = new THREE.MeshBasicMaterial({
        color: color,
        side: THREE.DoubleSide,
    });
    const q_board = new THREE.Mesh(q_board_geo, q_board_m);
    qb.add(q_board);

    let row_offset = 0;
    for (let i = 0; i < 10; i++) {
        let col_offset = 0;
        for (let j = 0; j < 10; j++) {
            const qb_cell_geo = new THREE.BoxGeometry(0.09, 0.09, 0.01);
            const qb_cell_m = new THREE.MeshBasicMaterial({ color: 0xc0c0c0 });
            const qb_cell = new THREE.Mesh(qb_cell_geo, qb_cell_m);
            qb_cell.position.set(-0.5 + col_offset, 0.5 + row_offset, 0.01);
            qb.add(qb_cell);
            clickables.push(qb_cell);
            col_offset += 0.11;
        }
        row_offset -= 0.11;
    }
    return qb;
}

/* toggle off when dragging game piece */
function toggle_orbit_controls(on_off) {
    if (on_off === "off") {
        controls.enablePan = false;
        controls.enableRotate = false;
        controls.enableZoom = false;
    } else {
        controls.enablePan = true;
        controls.enableRotate = true;
        controls.enableZoom = true;
    }
}

/* ui element overlay positioning helper */
function vw_to_local() {
    let j = 0;
    const fovRad = THREE.MathUtils.degToRad(camera.fov);
    const halfHeight = Math.tan(fovRad / 2) * -cam_offset;
    const halfWidth = halfHeight * camera.aspect;

    const p2_buttons_bbox = p2_buttons.getBoundingClientRect();
    const p1_buttons_bbox = p1_buttons.getBoundingClientRect();
    const player_name_bbox = player_name.getBoundingClientRect();
    const ai_name_bbox = ai_name.getBoundingClientRect();
    const screen_w = renderer.domElement.clientWidth;
    const screen_h = renderer.domElement.clientHeight;
    const p1b_pct = p1_buttons_bbox.width / screen_w;
    const player_box = player_name_bbox.height / screen_h;
    const p2b_pct = p1_buttons_bbox.width / screen_w;
    const ai_box = ai_name_bbox.height / screen_h;

    for (let i = 0; i < ui_elements.length; i++) {
        const obj = ui_elements[i];
        const btn_rect = i === 0 ? p1_buttons_bbox : p2_buttons_bbox;
        const name_rect = i === 0 ? player_name_bbox : ai_name_bbox;

        const vw = btn_rect.left / screen_w - 0.03;
        const vh = name_rect.bottom / screen_h + 0.03;

        const xCam = -halfWidth + vw * (halfWidth * 2);
        const yCam = halfHeight - vh * (halfHeight * 2);
        console.log("x: ", xCam, "y: ", yCam);

        obj.position.set(xCam, yCam, cam_offset);

        const screenPos = new THREE.Vector3();
        screenPos.copy(obj.position);
        screenPos.project(camera);
        const btext_w = xCam;
        const btext_h = yCam;
        j = j + 1;
    }
}

/* disable object group */
function toggle_element(obj, on_off) {
    let vis = true;
    if (on_off === "off") vis = false;
    obj.traverse(function (child) {
        if (child instanceof THREE.Mesh) {
            child.visible = vis;
        }
    });
}

/* get outline */
function outline_object(obj) {
    const outline = obj.clone();
    outline.material = new THREE.MeshBasicMaterial({
        color: 0xffffff,
        side: THREE.BackSide,
    });
    outline.position.copy(obj.position);

    outline.scale.multiplyScalar(1.2);
    return outline;
}

/* get highlight */
function highlight_patch(p) {
    if (current_highlight) scene.remove(current_highlight);
    const patch_color = p.children[0].material.color.getHex();
    console.log(patch_color);
    const complement = 0xffffff ^ patch_color;
    const color = new THREE.Color(complement);
    color.offsetHSL(0, 0, 0.2);
    const highlight = new THREE.Group();
    for (let i = 0; i < p.children.length; i++) {
        const hl = p.children[i].clone();
        hl.material = new THREE.MeshBasicMaterial({
            color: color,
            opacity: 0.8,
            transparent: true,
        });
        highlight.add(hl);
    }
    highlight.position.copy(p.position);
    highlight.position.z += 0.01;
    highlight.rotation.copy(p.rotation);
    current_highlight = highlight;
    scene.add(highlight);
}

let current_scene = scene;
let t = 0;
function animate() {
    t += 0.05;
    const pulse = 1 + Math.sin(t) * 0.05;
    b1.scale.set(pulse, pulse, pulse);
    b2.scale.set(pulse, pulse, pulse);
    if (skydome) skydome.rotation.y += 0.001;
    if (skydome2) skydome2.rotation.y += 0.001;
    patches2.rotation.y += 0.02;
    patches2.rotation.z += 0.02;
    patches2.rotation.x += 0.02;
    board2.rotation.z += 0.005;
    renderer.render(current_scene, camera);
}
renderer.setAnimationLoop(animate);

/* Bonsai entry points */

window.dimIneligiblePatches = function (pos) {
    console.log(pos);
    for (let i = 0; i < patches.children.length; i++) {
        if (i + 1 != pos && i + 1 != pos + 1 && i + 1 != pos + 2) {
            patches.children[i].traverse((obj) => {
                if (obj.isMesh) {
                    obj.material.transparent = true;
                    obj.material.opacity = 0.5;
                    obj.material.wireframe = true;
                }
            });
        } else {
            patches.children[i].traverse((obj) => {
                if (obj.isMesh) {
                    obj.material.transparent = false;
                    obj.material.opacity = 1;
                    obj.material.wireframe = false;
                }
            });
        }
    }
};
