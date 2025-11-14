/* Dallas Scott - ds4015
   Patchwork Three.JS UI elements
*/

import * as THREE from "three";
import { EffectComposer, OrbitControls } from "three/examples/jsm/Addons.js";
import { MTLLoader } from "three/examples/jsm/loaders/MTLLoader.js";
import { RenderPass } from "three/examples/jsm/postprocessing/RenderPass.js";
import { OutlinePass } from "three/examples/jsm/postprocessing/OutlinePass.js";
import { OBJLoader } from "three/examples/jsm/loaders/OBJLoader.js";
import { FontLoader } from "three/examples/jsm/loaders/FontLoader.js";
import { TextGeometry } from "three/examples/jsm/geometries/TextGeometry.js";
import * as BufferGeometryUtils from "three/examples/jsm/utils/BufferGeometryUtils.js";

import {
  CSS3DRenderer,
  CSS3DObject,
} from "three/examples/jsm/renderers/CSS3DRenderer.js";

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

let bonsaiLoaded = false;
const texLoader = new THREE.TextureLoader();

const canvas = document.getElementById("three-js-canvas");
const renderer = new THREE.WebGLRenderer({
  canvas: canvas,
  antialias: true,
  alpha: true,
});
const labelRenderer = new CSS3DRenderer();
labelRenderer.setSize(window.innerWidth, window.innerHeight);
labelRenderer.domElement.style.position = "absolute";
labelRenderer.domElement.style.top = "0";
labelRenderer.domElement.style.left = "0";
labelRenderer.domElement.style.pointerEvents = "none";
labelRenderer.domElement.style.zIndex = "10";
document.body.appendChild(labelRenderer.domElement);
renderer.setSize(window.innerWidth, window.innerHeight);
document.body.appendChild(renderer.domElement);
const controls = new OrbitControls(camera, renderer.domElement);
controls.target.set(0, 0, 0);
controls.update();
let camStart = new THREE.Vector3(0, 0, 0);
let camStartLookAt = new THREE.Vector3(0, 0, 0);
let camTarget = new THREE.Vector3(0, 0, 0);
let camTargetLookAt = new THREE.Vector3(0, 0, 0);
let currentLookAt = new THREE.Vector3(0, 0, 0);
let openAnimating = true;
let qbAnimating = false;
let tPulse = 0;
let tShift = 0;
let tSlide = 0;
camera.position.set(3.5, 0, 7.5);
moveCam(new THREE.Vector3(0, 0, 2.5), new THREE.Vector3(0, 0, 0));
camera2.position.z = 10.3;
camera2.position.y = 4;
const cam_offset = -2.3;
scene.add(camera);
scene2.add(camera2);
const p1_buttons = document.getElementById("p1-buttons");
const p2_buttons = document.getElementById("p2-buttons");
const player_name = document.getElementById("player-box-1");
const ai_name = document.getElementById("player-box-2");

let spinT = 0;
let spinAngle = 0;
let spinAnimating = false;
const ui_elements = [];
let ui_element_percentages = [];
let clickables = [];
let patch_clickables = [];
let draggable = [];
let ui_overlay_built = false;

/* logo */
const fontLoader = new FontLoader();
fontLoader.load("img3d/Princess Sofia_Regular.json", (font) => {
  const logo_g = new TextGeometry("Patchwork", {
    font: font,
    size: 0.25,
    height: 0.05,
    curveSegments: 12,
    bevelEnabled: true,
    bevelThickness: 0.01,
    bevelSize: 0.005,
    bevelSegments: 3,
  });

  logo_g.computeBoundingBox();
  logo_g.center();

  const logo_m = new THREE.MeshPhongMaterial({
    color: 0xffcc66,
    specular: 0x444444,
    shininess: 30,
  });

  const logo = new THREE.Mesh(logo_g, logo_m);
  logo.position.set(0, 0.8, 0);
  //scene.add(logo);
});

/* patch info overlay */
let PAR_TEXT_FONT = null;
let patch_info_1 = new THREE.Group();
let patch_info_2 = null;
let patch_info = patch_info_1;
fontLoader.load("img3d/Kranky_Regular.json", (font) => {
  PAR_TEXT_FONT = font;
  const patch_inf_g = new TextGeometry("Cost: \n\nTime: \n\nIncome: ", {
    font: font,
    size: 0.05,
    depth: 0.005,
    curveSegments: 24,
    bevelEnabled: true,
    bevelThickness: 0.01,
    bevelSize: 0.005,
    bevelSegments: 5,
  });
  patch_inf_g.computeBoundingBox();
  patch_inf_g.center();
  const patch_inf_m = new THREE.MeshPhongMaterial({
    color: 0xffffff,
    specular: 0xffffff,
    shininess: 10,
  });
  const textLight1 = new THREE.PointLight(0xffffff, 0.8, 2);
  textLight1.position.set(-3, 0.2, 1.5);
  scene.add(textLight1);
  const patch_inf = new THREE.Mesh(patch_inf_g, patch_inf_m);
  // patch_inf.position.set(-3.3, 0.1, 0);
  patch_info_1.add(patch_inf);
  const values_text = get_patch_values();
  console.log(values_text);
  if (values_text) patch_info_1.add(values_text);
});

const button_cost_icon = make_button(0, 0, false);
button_cost_icon.scale.set(0.5, 0.5, 0.5);
button_cost_icon.position.set(0.15, 0.18, 0);
patch_info_1.add(button_cost_icon);

const hourglass_sand_geo = new THREE.ConeGeometry(1, 2.5, 25, 25);
const hourglass_sand_m = new THREE.PointsMaterial({
  color: 0xc2b280,
  size: 0.0007,
});
const woodColor = texLoader.load("img3d/wood-color.jpg");
const woodNormal = texLoader.load("img3d/wood-normal.jpg");
const woodRough = texLoader.load("img3d/wood-roughness.jpg");

woodColor.wrapS = woodColor.wrapT = THREE.RepeatWrapping;
woodColor.repeat.set(2, 2);

const woodMat = new THREE.MeshStandardMaterial({
  map: woodColor,
  normalMap: woodNormal,
  roughnessMap: woodRough,
  roughness: 0.4,
  metalness: 0.0,
});

const hourglass = new THREE.Group();
const hourglass_sand = new THREE.Points(hourglass_sand_geo, hourglass_sand_m);
const hourglass_geo = new THREE.ConeGeometry(1.4, 2.5, 64, 1);
const glassMat = new THREE.MeshPhysicalMaterial({
  color: 0xffffff,
  roughness: 0.05,
  metalness: 0,
  transmission: 1,
  thickness: 0.4,
  ior: 1.5,
  transparent: true,
  envMapIntensity: 1.0,
});
const hg_wood_geo = new THREE.BoxGeometry(3, 3, 0.5);
const hg_wood_top = new THREE.Mesh(hg_wood_geo, woodMat);
const hg_wood_bot = new THREE.Mesh(hg_wood_geo, woodMat);
hg_wood_top.rotation.x += Math.PI / 2;
hg_wood_top.position.y = -1.5;
hg_wood_bot.rotation.x += Math.PI / 2;
hg_wood_bot.position.y = 3.502;
const hourglass_bottom = new THREE.Mesh(hourglass_geo, glassMat);
const hourglass_top = new THREE.Mesh(hourglass_geo, glassMat);
hourglass_top.rotation.x += Math.PI;
hourglass_top.position.y = 2;
hourglass.add(hourglass_sand);
hourglass.add(hourglass_bottom);
hourglass.add(hourglass_top);
hourglass.add(hg_wood_top);
hourglass.add(hg_wood_bot);
hourglass.scale.set(0.017, 0.017, 0.017);
hourglass.position.z = 0;
hourglass.position.x = 0.15;
hourglass.position.y = -0.037;
patch_info_1.add(hourglass);

patch_info_1.position.set(-3.1, 0.16, 0.1);
scene.add(patch_info);
patch_info.visible = false;
const patch_info_wobble = {
  rot: 0.3,
  amp: 0.05,
  speed: 5,
};

/* responsive canvas */
window.addEventListener("resize", onWindowResize);

const TARGET_ASPECT = 16 / 9;
function onWindowResize() {
  const vw = window.innerWidth;
  const width = vw;
  const height = Math.round(width / TARGET_ASPECT);

  camera.aspect = width / height;
  camera.updateProjectionMatrix();
  camera2.aspect = width / height;
  camera2.updateProjectionMatrix();

  renderer.setSize(width, height);
  labelRenderer.setSize(width, height);

  const vh = window.innerHeight;
  const topOffset = Math.round((vh - height) / 2);

  renderer.domElement.style.width = width + "px";
  renderer.domElement.style.height = height + "px";
  renderer.domElement.style.top = "0px";
  renderer.domElement.style.left = "0px";

  labelRenderer.domElement.style.width = width + "px";
  labelRenderer.domElement.style.height = height + "px";
  labelRenderer.domElement.style.top = topOffset + "px";
  labelRenderer.domElement.style.left = "0px";

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

/* neut token model */
let neutral_token;
let neut_init_pos;
objLoader.load("img3d/neut.obj", (obj) => {
  obj.traverse((child) => {
    if (child.isMesh) {
      child.material = new THREE.MeshBasicMaterial({
        color: 0xfff8dc,
      });
    }
  });

  obj.scale.set(0.03, 0.03, 0.03);
  obj.rotation.x += Math.PI / 2;
  obj.position.x = 1.2;
  obj.position.z = 0.2;
  obj.position.y = -0.23;
  neutral_token = obj;
  window.moveNeutralToken(neut_init_pos);
  patches.add(neutral_token);
});

/* quilt board close buttons */
const close_button_1 = new THREE.Group();
const close_button_2 = new THREE.Group();
const big_back_g = new THREE.BoxGeometry(0.07, 0.07, 0.02);
const big_back_m = new THREE.MeshBasicMaterial({ color: 0x9b9bc2 });
const little_back_g = new THREE.BoxGeometry(0.06, 0.06, 0.02);
const little_back_m = new THREE.MeshBasicMaterial({ color: 0xff004d });
const big_back_1 = new THREE.Mesh(big_back_g, big_back_m);
const little_back_1 = new THREE.Mesh(little_back_g, little_back_m);
const big_back_2 = new THREE.Mesh(big_back_g, big_back_m);
const little_back_2 = new THREE.Mesh(little_back_g, little_back_m);

big_back_1.position.set(0, 0, 0);
little_back_1.position.set(0, 0, 0.01);
big_back_2.position.set(0, 0, 0);
little_back_2.position.set(0, 0, 0.01);
close_button_1.add(big_back_1);
close_button_1.add(little_back_1);
close_button_2.add(big_back_2);
close_button_2.add(little_back_2);
close_button_1.position.set(-2.8, 0.65, 0);
close_button_2.position.set(2.8, 0.65, 0);

let current_close_button = close_button_1;

/* UI button currency icons */
const b1 = make_button(0.05, 0.15, true);
b1.rotation.y += 0.18;
b1.rotation.z += 0.09;
const b2 = make_button(0.9, 0.15, true);
b2.rotation.y -= 0.18;
b2.rotation.z -= 0.09;
let rot_ct = 1;
const button_cache = new THREE.Group();
make_and_place_button_cache();
function make_and_place_button_cache() {
  for (let i = 0; i < 15; i++) {
    const stack = make_button_stack(10);
    button_cache.add(stack);
  }

  for (let i = 0; i < button_cache.children.length; i++) {
    let alpha = Math.PI / 2 - 0.418879 * i;
    let x = Math.cos(alpha) * 1.1;
    let z = Math.sin(alpha) * 1.1;
    button_cache.children[i].position.set(x, -1.2, z);
  }
}
const stack1 = make_button_stack(10);
stack1.position.set(0, -1.2, 0.7);
//scene.add(button_cache);
function make_button_stack(num) {
  const stack = new THREE.Group();
  for (let i = 0; i < num; i++) {
    const jitter = THREE.MathUtils.randFloat(-0.02, 0.02);
    const button = make_button(jitter, i * 0.022, false);
    stack.add(button);
  }
  return stack;
}

function make_button(x_pct, y_pct, for_ui) {
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

  if (for_ui) {
    ui_elements.push(button);
    ui_element_percentages.push(x_pct);
    ui_element_percentages.push(y_pct);
    camera.add(button);
    vw_to_local();
  } else {
    button.position.set(x_pct, y_pct, 0);
    button.rotation.x -= Math.PI / 2;
  }
  return button;
}

let tFlip = 0;
let flipAnimating = false;
let flipping = null;

function makeButtonsToFlip(num) {
  const buttons = new THREE.Group();

  for (let i = 0; i < num; i++) {
    let sign;
    const y_off = THREE.MathUtils.randFloat(-0.08, 0.08);
    if (i % 2 === 0) {
      sign = 1;
    } else sign = -1;
    const button = make_button(0, 0, false);
    button.position.set(sign * 0.12 * i, -2 + y_off * i, -2);
    buttons.add(button);
  }
  camera.add(buttons);
  flipping = buttons;
  tFlip = 0;
}

/* main board */
const geometry = new THREE.BoxGeometry(1.1, 1.1, 0.01);
const material = new THREE.MeshBasicMaterial({ color: 0x00ff00 });
const texture = texLoader.load("img3d/bg.png");
texture.colorSpace = THREE.SRGBColorSpace;
const mat = new THREE.MeshBasicMaterial({ map: texture });
const cube = new THREE.Mesh(geometry, mat);

const main_board_cells = [];
const board_cell_position_numbers = [];
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
  for (let i = 6; i < 14; i++) board_cell_position_numbers.push(i);
  full_singles_row(8, 2);
  board_cell_position_numbers.push(4);

  board_cell_position_numbers.push(30);
  for (let i = 46; i < 50; i++) board_cell_position_numbers.push(i);
  board_cell_position_numbers.push(36);
  board_cell_position_numbers.push(15);

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
  main_board_cells.push(r2c3to3p5);
  main_board_cells.push(r2c3p5to5);
  main_board_cells.push(r2c6);
  main_board_cells.push(r2c7);
  main_board_cells.push(r2c8);
  board_cell_position_numbers.push(5);
  for (let i = 31; i < 36; i++) board_cell_position_numbers.push(i);
  board_cell_position_numbers.push(14);

  // row 4
  const r4c1 = new THREE.Mesh(single, m);
  r4c1.position.set(-0.455 + 0 * 0.13, 0.455 - 0.13 * 3);
  const r4c2 = new THREE.Mesh(single, m);
  r4c2.position.set(-0.455 + 1 * 0.13, 0.455 - 0.13 * 3);
  const r4c3 = new THREE.Mesh(double, m);
  r4c3.position.set(-0.455 + 2 * 0.13, 0.455 - 0.13 * 3.25);
  r4c3.rotation.z = 1.57079;
  const r4c4 = new THREE.Mesh(end_zone, m);
  r4c4.position.set(-0.455 + 3.49 * 0.13, 0.455 - 0.13 * 3.49);
  const r4c6 = new THREE.Mesh(double, m);
  r4c6.position.set(-0.455 + 5 * 0.13, 0.455 - 0.13 * 3.25);
  r4c6.rotation.z = 1.57079;
  const r4c7 = new THREE.Mesh(single, m);
  r4c7.position.set(-0.455 + 6 * 0.13, 0.455 - 0.13 * 3);
  const r4c8 = new THREE.Mesh(single, m);
  r4c8.position.set(-0.455 + 7 * 0.13, 0.455 - 0.13 * 3);

  board.add(r4c1);
  board.add(r4c2);
  board.add(r4c3);
  board.add(r4c4);
  board.add(r4c6);
  board.add(r4c7);
  board.add(r4c8);
  main_board_cells.push(r4c1);
  main_board_cells.push(r4c2);
  main_board_cells.push(r4c3);
  main_board_cells.push(r4c4);
  main_board_cells.push(r4c6);
  main_board_cells.push(r4c7);
  main_board_cells.push(r4c8);
  board_cell_position_numbers.push(3);
  board_cell_position_numbers.push(29);
  board_cell_position_numbers.push(45);
  board_cell_position_numbers.push(54);
  board_cell_position_numbers.push(50);
  board_cell_position_numbers.push(37);
  board_cell_position_numbers.push(16);

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
  board_cell_position_numbers.push(2);
  board_cell_position_numbers.push(28);
  board_cell_position_numbers.push(44);
  board_cell_position_numbers.push(51);
  board_cell_position_numbers.push(38);
  board_cell_position_numbers.push(17);

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
  board_cell_position_numbers.push(1);
  board_cell_position_numbers.push(27);
  board_cell_position_numbers.push(53);
  board_cell_position_numbers.push(52);
  board_cell_position_numbers.push(39);
  board_cell_position_numbers.push(18);

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
  board_cell_position_numbers.push(26);
  for (let i = 43; i > 39; i--) board_cell_position_numbers.push(i);
  board_cell_position_numbers.push(19);

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
  for (let i = 25; i > 19; i--) board_cell_position_numbers.push(i);

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

scene.background = new THREE.Color(0xa873ef);
const dirLight = new THREE.DirectionalLight(0xffffff, 1.5);
dirLight.position.set(0, 1, 1);
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
draggable.push(p2_tt);
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
//p2_tt.add(tt2_outline);

/* patches */
let patches_built = false;
let rotation_count = 0;

const patches = new THREE.Group();

let patch_clone = null;
let patches2 = patches.clone(true);
scene2.add(patches2);

function create_patches(
  patch_dimensions,
  patch_cols,
  patch_costs,
  patch_times,
  patch_incomes,
) {
  const patch_cell_geo = new THREE.BoxGeometry(0.06, 0.06, 0.012);

  for (let i = 0; i < patch_dimensions.length; i++) {
    const randomColor = Math.floor(Math.random() * 0xffffff);
    const patch_material = new THREE.MeshBasicMaterial({
      color: randomColor,
    });
    for (let j = 0; j < patch_dimensions[i].length - 1; j += 2) {
      const num_cubes = patch_dimensions[i][j];
      const direction = patch_dimensions[i][j + 1];
    }
    let up_offset = 0;
    let down_offset = 0;
    let left_offset = 0;
    let right_offset = 0;
    const o = 0.06;

    const patch = new THREE.Group();
    patch.userData.original_color = randomColor;
    const initial_patch_cell = new THREE.Mesh(patch_cell_geo, patch_material);
    patch.add(initial_patch_cell);
    for (let j = 0; j < patch_dimensions[i].length - 1; j += 2) {
      const num_cubes = patch_dimensions[i][j];
      const direction = patch_dimensions[i][j + 1];
      for (let k = 0; k < num_cubes; k++) {
        const patch_cell = new THREE.Mesh(patch_cell_geo, patch_material);
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
        patch.userData = {
          pos: i + 1,
          cost: patch_costs[i],
          time: patch_times[i],
          income: patch_incomes[i],
          orig_rot: patch.rotation.clone(),
        };

        patches.add(patch);
      }
    }
    current_scene.add(patches);
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
function get_patch_values() {
  if (!placing) return;
  const cost_val = manipulating.userData.cost;
  const time_val = manipulating.userData.time;
  const income_val = manipulating.userData.income;
  const values_text = `${cost_val}\n\n${time_val}`;
  const income_text = `${income_val}`;
  console.log(cost_val, ", ", time_val, ", ", income_val);
  const patch_inf_g = new TextGeometry(values_text, {
    font: PAR_TEXT_FONT,
    size: 0.05,
    depth: 0.005,
    curveSegments: 24,
    bevelEnabled: true,
    bevelThickness: 0.01,
    bevelSize: 0.005,
    bevelSegments: 5,
  });

  const patch_inf_m = new THREE.MeshPhongMaterial({
    color: 0xdfaf35,
    specular: 0xffffff,
    shininess: 30,
  });
  const patch_inf_g2 = new TextGeometry(income_text, {
    font: PAR_TEXT_FONT,
    size: 0.05,
    depth: 0.005,
    curveSegments: 24,
    bevelEnabled: true,
    bevelThickness: 0.01,
    bevelSize: 0.005,
    bevelSegments: 5,
  });
  patch_inf_g.computeBoundingBox();
  patch_inf_g.center();
  const income_value_text = new THREE.Mesh(patch_inf_g2, patch_inf_m);
  const patch_values_text = new THREE.Mesh(patch_inf_g, patch_inf_m);
  income_value_text.userData.role = "value";
  patch_values_text.userData.role = "value";
  patch_values_text.position.set(0.3, 0.07, 0.1);
  income_value_text.position.set(0.275, -0.21, 0.1);
  //  patch_values_text.scale.set(0.15, 0.15, 0.15);
  const player_turn = getPlayerTurn();
  if (player_turn === 1) {
    patch_info_1.add(patch_values_text);
    patch_info_1.add(income_value_text);
  } else {
    if (!patch_info_2) {
      patch_info_2 = patch_info_1.clone(true);
      patch_info_2.position.set(1.2, 0.25, 0.02);
      scene.add(patch_info_2);
    }
    patch_info_2.add(patch_values_text);
    patch_info_2.add(income_value_text);
  }
}

/* quilt boards */
const p1_quilt_board = create_quilt_board(new THREE.Color(0x0000ff));
p1_quilt_board.position.x = -2.2;
scene.add(p1_quilt_board);
const p2_quilt_board = create_quilt_board(new THREE.Color(0xff0000));
p2_quilt_board.position.x = 2.2;
scene.add(p2_quilt_board);
let current_quilt_board = p1_quilt_board;

function create_quilt_board(color) {
  const qb = new THREE.Group();
  const q_board_geo = new THREE.PlaneGeometry(1.1, 1.1);
  const q_board_m = new THREE.MeshBasicMaterial({
    color: color,
    side: THREE.DoubleSide,
  });
  const q_board = new THREE.Mesh(q_board_geo, q_board_m);
  qb.add(q_board);

  let row_offset = -0.055;
  for (let i = 1; i < 10; i++) {
    let col_offset = 0.055;
    for (let j = 1; j < 10; j++) {
      const qb_cell_geo = new THREE.BoxGeometry(0.09, 0.09, 0.01);
      const qb_cell_m = new THREE.MeshBasicMaterial({ color: 0xc0c0c0 });
      const qb_cell = new THREE.Mesh(qb_cell_geo, qb_cell_m);
      qb_cell.userData = { row: i, col: j };
      qb_cell.position.set(-0.5 + col_offset, 0.5 + row_offset, 0.01);
      qb.add(qb_cell);
      col_offset += 0.11;
    }
    row_offset -= 0.11;
  }
  return qb;
}

/* drag tokens */
let manipulating = null;
let dragging = false;
let placing = false;
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
  if (event.button != 0) return;
  const rect = renderer.domElement.getBoundingClientRect();

  mouse.x = ((event.clientX - rect.left) / rect.width) * 2 - 1;
  mouse.y = -((event.clientY - rect.top) / rect.height) * 2 + 1;

  raycaster.setFromCamera(mouse, camera);
  const hits = raycaster.intersectObjects(clickables, true);
  const patch_hits = raycaster.intersectObjects(patch_clickables, true);
  const drag_hits = raycaster.intersectObjects(draggable, true);
  const close_button_hits = raycaster.intersectObject(
    current_close_button,
    true,
  );
  const quilt_cell_hits = raycaster.intersectObjects(
    current_quilt_board.children,
    true,
  );

  if (object_outlined) {
    scene.remove(object_outlined);
    object_outlined.geometry.dispose();
    object_outlined.material.dispose();
    object_outlined = null;
  }

  /* patch select overlay */
  if (patch_hits.length > 0) {
    const chosen = patch_hits[0].object;
    if (chosen.parent.visible == false || chosen.material.wireframe == true)
      return;
    placing = true;
    let camTarget;
    let camTargetLookAt;
    const player_turn = getPlayerTurn();
    console.log("player turn: ", player_turn);
    if (player_turn === 1) {
      patch_info = patch_info_1;
      camTarget = new THREE.Vector3(-2.2, 0, 1);
      camTargetLookAt = new THREE.Vector3(-2.2, 0, 0);
      current_quilt_board = p1_quilt_board;
      current_close_button = close_button_1;
    } else {
      camTarget = new THREE.Vector3(2.2, 0, 1);
      camTargetLookAt = new THREE.Vector3(2.2, 0, 0);
      current_quilt_board = p2_quilt_board;
      current_close_button = close_button_2;
    }
    tSlide = 0;
    qbAnimating = true;
    moveCam(camTarget, camTargetLookAt);
    patch_clone = chosen.parent.clone(true);
    patch_clone.traverse((obj) => {
      if (obj.isMesh) {
        obj.material = obj.material.clone();
      }
    });
    patch_clone.rotation.set(chosen.parent.userData.orig_rot);
    patch_clone.position.set(-2.2, 0, 0.02);
    patch_clone.rotation.set(0, 0, 0);
    patch_clone.rotation.z = -Math.PI;
    patch_clone.rotation.y = -Math.PI;
    patch_clone.scale.set(1.8, 1.8, 0);
    dragZ = 0.02;
    scene.remove(chosen);
    scene.add(patch_clone);
    manipulating = patch_clone;
    get_patch_values();
    if (player_turn === 2) {
      patch_info = patch_info_2;
      patch_info.visible = true;
    } else {
      patch_info = patch_info_1;
      patch_info.visible = true;
    }
    scene.add(current_close_button);
    toggle_orbit_controls("off");
  }

  /* patch place on quilt board cell */
  if (quilt_cell_hits.length > 0) {
    const hit = quilt_cell_hits.find((h) => h.object.userData);
    const cell = hit.object;
    const row = cell.userData.row;
    const col = cell.userData.col;
    const patch = manipulating.userData.pos;
    const time = manipulating.userData.time;
    const place_res = window.bonsaiPlacePatch(row, col, patch, time);
    if (place_res) {
      current_quilt_board.add(manipulating);
      manipulating.position.copy(cell.position);
      manipulating.position.z = dragZ;
      placing = false;
      removeCirclePatch(patch);
      manipulating = null;
      rotation_count = 0;
      const turn = getPlayerTurn();
      if (turn === 1) {
        current_quilt_board = p1_quilt_board;
        current_close_button = close_button_1;
      } else {
        current_quilt_board = p2_quilt_board;
        current_close_button = close_button_2;
      }
      close_window(false);
    }
  }

  /* drag time token */
  if (drag_hits.length > 0) {
    const hit = drag_hits[0];

    dragging = true;
    manipulating = hit.object;
    manipulating.scale.multiplyScalar(1.5);
    dragZ = manipulating.position.z;
    toggle_orbit_controls("off");
    manipulating.getWorldPosition(worldPos);
    dragOffset.copy(hit.point).sub(worldPos);
  }

  if (close_button_hits.length > 0) {
    close_window(true);
  }
});

renderer.domElement.addEventListener("contextmenu", (e) => e.preventDefault());
renderer.domElement.addEventListener("pointerdown", (e) => {
  if (e.button === 2) {
    if (placing) {
      rotation_count += 1;
      let pnum = manipulating.userData.pos;
      window.updatePatchRotation(pnum, rotation_count);
      patch_clone.rotation.z += Math.PI / 2;
      if (patch_clone.rotation.z < 0) patch_clone.rotation.z -= 2 * Math.PI;
    }
  }
});

renderer.domElement.addEventListener("pointerup", () => {
  if (!dragging) return;
  if (manipulating != null) {
    get_cell_under_token(manipulating);
    if (manipulating != null) manipulating.scale.multiplyScalar(1 / 1.5);
  }
  manipulating = null;
  toggle_orbit_controls("on");
  dragging = false;
});

renderer.domElement.addEventListener("pointermove", (event) => {
  if (!manipulating) return;

  const rect = renderer.domElement.getBoundingClientRect();

  mouse.x = ((event.clientX - rect.left) / rect.width) * 2 - 1;
  mouse.y = -((event.clientY - rect.top) / rect.height) * 2 + 1;

  raycaster.setFromCamera(mouse, camera);

  if (raycaster.ray.intersectPlane(intersectionPlane, intersectionPoint)) {
    intersectionPoint.sub(dragOffset);
    const parent = manipulating.parent;
    if (parent) {
      parent.worldToLocal(intersectionPoint);
      intersectionPoint.z = dragZ;
      manipulating.position.copy(intersectionPoint);
    } else {
      intersectionPoint.z = dragZ;
      manipulating.position.copy(intersectionPoint);
    }
  }
});

/* close quilt board/patch place view */
function close_window(destroy) {
  const toDestroy = [];
  patch_info_1.traverse((obj) => {
    if (obj.isMesh && obj.userData.role === "value") {
      toDestroy.push(obj);
    }
  });
  if (patch_info_2) {
    patch_info_2.traverse((obj) => {
      if (obj.isMesh && obj.userData.role === "value") {
        toDestroy.push(obj);
      }
    });
  }
  toDestroy.forEach((mesh) => {
    destroy_mesh(mesh);
  });
  patch_info.visible = false;
  rotation_count = 0;
  tSlide = 0;
  qbAnimating = true;
  moveCam(new THREE.Vector3(0, 0, 2.3), new THREE.Vector3(0, 0, 0), false);
  placing = false;
  if (destroy) destroy_mesh(patch_clone);
  scene.remove(current_close_button);
  toggle_orbit_controls("on");
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

/* delete mesh, free resources, remove from scene */
function destroy_mesh(mesh) {
  if (mesh.isGroup) {
    mesh.traverse((obj) => {
      if (obj.isMesh) {
        if (obj.geometry) obj.geometry.dispose();
        if (obj.material) {
          if (Array.isArray(obj.material)) {
            obj.material.forEach((m) => m.dispose());
          } else {
            obj.material.dispose();
          }
        }
      }
    });
    if (mesh.parent) mesh.parent.remove(mesh);
  } else {
    if (mesh.isMesh) {
      if (mesh.geometry) mesh.geometry.dispose();
      if (mesh.material) {
        if (Array.isArray(mesh.material)) {
          mesh.material.forEach((m) => m.dispose());
        } else {
          mesh.material.dispose();
        }
      }
    }
    if (mesh.parent) mesh.parent.remove(mesh);
  }
}

function removeCirclePatch(pos) {
  for (let i = 0; i < patches.children.length; i++) {
    const group = patches.children[i];
    if (group.userData && group.userData.pos === pos) {
      group.visible = false;
      break;
    }
  }
}

function get_cell_under_token(tk) {
  const token_pos = tk.position;
  let found = false;
  for (let i = 0; i < main_board_cells.length; i++) {
    const box = new THREE.Box3().setFromObject(main_board_cells[i]);

    if (
      token_pos.x >= box.min.x &&
      token_pos.x <= box.max.x &&
      token_pos.y >= box.min.y &&
      token_pos.y <= box.max.y
    ) {
      window.bonsaiCheckTokenPosition(board_cell_position_numbers[i]);
      found = true;
      break;
    }
  }
  if (!found) window.bonsaiCheckTokenPosition(-1);
}

function moveCam(pos, lookAt) {
  camStart.copy(camera.position);
  camStartLookAt.copy(currentLookAt);
  camTarget.copy(pos);
  camTargetLookAt.copy(lookAt);
}

function build_ui_overlay() {
  const div_cont = document.createElement("div");
  const div_info = document.createElement("div");
  div_info.className = "patch-info";
  div_info.id = "patch-info";
  const div_attrs = document.createElement("div");
  div_attrs.className = "div-attrs";
  const div_cost_label = document.createElement("div");
  const div_cost_value = document.createElement("div");
  const div_time_label = document.createElement("div");
  const div_time_value = document.createElement("div");

  div_cost_label.className = "patch-label";
  div_cost_label.textContent = "Cost: ";
  div_cost_value.className = "patch-value";
  div_cost_value.id = "patch-cost";
  div_time_label.className = "patch-label";
  div_time_label.textContent = "Time: ";
  div_time_value.className = "patch-value";
  div_time_value.id = "patch-time";

  div_attrs.appendChild(div_cost_label);
  div_attrs.appendChild(div_cost_value);
  div_attrs.appendChild(div_time_label);
  div_attrs.appendChild(div_time_value);

  div_info.appendChild(div_attrs);

  div_cont.appendChild(div_info);

  const patch_info_1 = new CSS3DObject(div_cont);

  patch_info_1.position.set(-3, 0.25, 0.02);
  patch_info_1.scale.set(0.0025, 0.0025, 0.0025);

  // scene.add(patch_info_1);
  ui_overlay_built = true;
}

function get_mb_cell_from_pos(pos) {
  let index = 0;
  for (let i = 0; i < board_cell_position_numbers.length; i++)
    if (board_cell_position_numbers[i] === pos) index = i;
  return main_board_cells[index];
}

/* get highlight */
function highlight_patch(p) {
  if (current_highlight) scene.remove(current_highlight);
  const patch_color = p.children[0].material.color.getHex();
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

function getPlayerTurn() {
  return window.queryPlayerTurn();
}

function position_token(pnum, pos) {
  const grid_cell = get_mb_cell_from_pos(pos);
  let token = p1_tt;
  if (pnum === 2) token = p2_tt;
  token.position.copy(grid_cell.position);
  token.position.z = 0.025;
}

let current_scene = scene;

/* render loop */
function animate() {
  if (openAnimating || qbAnimating) {
    if (openAnimating) {
      const openSpeed = 0.009;
      tShift += openSpeed;
      if (tShift >= 1) {
        tShift = 1;
        openAnimating = false;
      }
    }
    if (qbAnimating) {
      const qbSpeed = 0.03;
      tSlide += qbSpeed;
      if (tSlide >= 1) {
        tSlide = 1;
        qbAnimating = false;
      }
    }
    let t;
    if (qbAnimating || tSlide === 1) t = tSlide;
    else t = tShift;
    camera.position.lerpVectors(camStart, camTarget, t);
    currentLookAt.lerpVectors(camStartLookAt, camTargetLookAt, t);
    camera.lookAt(currentLookAt);
  }
  if (flipAnimating) {
    const flipSpeed = 0.002;
    const player_turn = getPlayerTurn();
    for (let i = 0; i < flipping.children.length; i++) {
      const x_off = THREE.MathUtils.randFloat(0.01, 0.02);
      let sign = -1;
      if (player_turn === 2) sign = 1;
      flipping.children[i].position.y += 0.03;
      flipping.children[i].rotation.x += 0.2;
      flipping.children[i].position.x += sign * x_off;
    }
    tFlip += flipSpeed;
    if (tFlip >= 1) {
      tFlip = 1;
      flipAnimating = false;
    }

    if (spinAnimating) {
      const radius = 2.3;
      const steps = 30;
      const spinSpeed = 1 / steps;
      let deltaRot = (2 * Math.PI) / steps;
      spinT += spinSpeed;
      spinAngle += deltaRot;
      camera.position.x = radius * Math.cos(spinAngle);
      camera.position.z = radius * Math.sin(spinAngle);
      camera.lookAt(0, 0, 0);

      if (spinT >= 1) {
        spinAnimating = false;
        spinT = 0;
        camera.position.x = 0;
        camera.position.z = 2.3;
        camera.rotation.y = 0;
        spinAngle = 0;
      }
    }
  }
  hourglass.rotation.y += 0.04;

  const wobbleT = performance.now() * 0.001;
  patch_info.rotation.y =
    patch_info_wobble.rot +
    Math.sin(wobbleT * patch_info_wobble.speed) * patch_info_wobble.amp;

  tPulse += 0.05;
  const pulse = 1 + Math.sin(tPulse) * 0.05;

  b1.scale.set(pulse, pulse, pulse);
  b2.scale.set(pulse, pulse, pulse);
  if (skydome) skydome.rotation.y += 0.001;
  if (skydome2) skydome2.rotation.y += 0.001;
  patches2.rotation.y += 0.02;
  patches.rotation.z += 0.002;

  patches2.rotation.x += 0.02;
  board2.rotation.z += 0.005;
  hourglass_sand.rotation.y += 0.01;
  renderer.render(current_scene, camera);
  labelRenderer.render(current_scene, camera);
}
renderer.setAnimationLoop(animate);

/* Bonsai entry points */

window.dimIneligiblePatches = function (pos) {
  while (!patches) {
    continue;
  }
  function dim(p) {
    patches.children[p].traverse((obj) => {
      if (obj.isMesh) {
        obj.material.transparent = true;
        obj.material.opacity = 0.3;
        obj.material.wireframe = true;
      }
    });
  }

  function show(p) {
    patches.children[p].traverse((obj) => {
      if (obj.isMesh) {
        obj.material.transparent = false;
        obj.material.opacity = 1;
        obj.material.wireframe = false;
      }
    });
  }

  let ct = 0;

  let k = pos;
  console.log(k);
  if (k > 32 && patches.children.length > 0) k = 0;
  while (patches.children[k].visible === false) {
    k++;
  }
  let patch1 = patches.children[k++];
  if (k > 32 && patches.children.length > 0) k = 0;
  console.log(k);
  while (patches.children[k].visible === false) {
    k++;
  }
  let patch2 = patches.children[k++];
  if (k > 32 && patches.children.length > 0) k = 0;
  console.log(k);
  while (patches.children[k].visible === false) {
    k++;
  }
  let patch3 = patches.children[k];

  console.log(
    "patch1: ",
    patch1.userData.pos,
    ", patch2: ",
    patch2.userData.pos,
    "patch3: ",
    patch3.userData.pos,
  );

  for (let i = 0; i < patches.children.length; i++) {
    const child = patches.children[i];
    const p = child.userData && child.userData.pos;
    if (
      p === patch1.userData.pos ||
      p === patch2.userData.pos ||
      p === patch3.userData.pos
    )
      show(i);
    else dim(i);
  }
};

window.playButtonFlipAnimation = function (num) {
  if (flipping) {
    destroy_mesh(flipping);
    flipping = null;
    flipAnimating = false;
  }
  makeButtonsToFlip(num);
  flipAnimating = true;
};

window.repositionTimeTokens = function (p1, p2) {
  const p1_grid_cell = get_mb_cell_from_pos(p1);
  const p2_grid_cell = get_mb_cell_from_pos(p2);
  p1_tt.position.copy(p1_grid_cell.position);
  p2_tt.position.copy(p2_grid_cell.position);
  p1_tt.position.z = 0.025;
  p2_tt.position.z = 0.025;
  if (p1 === p2) {
    p1_tt.position.y += 0.03;
    p2_tt.position.y -= 0.03;
  }
};

window.placePatchesOnQuiltBoard = function (patches, rows, cols) {};

window.moveNeutralTokenInitial = function (pos) {
  if (!neutral_token) return;
  let displayPos = pos - 1;
  if (displayPos < 1) displayPos = 33;

  for (let i = 0; i < patches.children.length; i++) {
    const child = patches.children[i];
    if (child.userData && child.userData.pos === displayPos) {
      neutral_token.position.copy(child.position);
      break;
    }
  }
};

window.placeAIPatch = function (patch_num, row, col) {
  let patch;
  let cell;
  console.log("placing patch ", patch_num, " at ", row, ", ", col);
  for (let i = 0; i < patches.children.length; i++) {
    if (i === patch_num - 1) {
      patch = patches.children[i];
    }
  }
  for (let i = 0; i < p2_quilt_board.children.length; i++) {
    if (
      p2_quilt_board.children[i].userData.row === row &&
      p2_quilt_board.children[i].userData.col === col
    )
      cell = p2_quilt_board.children[i];
  }
  const patch_clone = patch.clone(true);
  patch_clone.scale.set(1.8, 1.8, 1.8);
  patch_clone.traverse((o) => {
    if (o.isMesh) {
      o.material = o.material.clone();
      o.material.transparent = false;
      o.material.opacity = 1;
      o.material.wireframe = false;
    }
  });
  patch_clone.visible = true;
  patch_clone.position.copy(cell.position);
  patch_clone.position.z = (patch_clone.position.z ?? 0) + 0.02;
  patch_clone.rotation.copy(patch.userData.orig_rot);
  p2_quilt_board.add(patch_clone);
  patch.visible = false;
};

window.moveNeutralToken = function (pos) {
  if (!neutral_token) return;

  for (let i = 0; i < patches.children.length; i++) {
    const child = patches.children[i];
    if (child.userData && child.userData.pos === pos) {
      neutral_token.position.copy(child.position);
      break;
    }
  }
};

window.buildInitialPatches = function (pl, pr, pc, pt, pi, n) {
  create_patches(pl, pr, pc, pt, pi);
  neut_init_pos = n;
};

window.window.onBonsaiReady = function () {
  //if (ui_overlay_built) return;
  //build_ui_overlay();
  build;
};
