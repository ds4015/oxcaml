/* Dallas Scott (ds4015)
   multiplayer.js - Firebase for multiplayer
*/
import { initializeApp } from "https://www.gstatic.com/firebasejs/11.0.1/firebase-app.js";
import {
  getAuth,
  signInAnonymously,
  onAuthStateChanged,
} from "https://www.gstatic.com/firebasejs/11.0.1/firebase-auth.js";
import {
  getFirestore,
  doc,
  setDoc,
  getDoc,
  onSnapshot,
  collection,
  query,
  orderBy,
  serverTimestamp,
} from "https://www.gstatic.com/firebasejs/11.0.1/firebase-firestore.js";
import {
  getAnalytics,
  logEvent,
  isSupported,
} from "https://www.gstatic.com/firebasejs/11.0.1/firebase-analytics.js";

const firebaseConfig = {
  apiKey: "AIzaSyDwvVvut-SP_76VHU4vjbnalZkWVmQja_g",
  authDomain: "patchwork-4e0aa.firebaseapp.com",
  projectId: "patchwork-4e0aa",
  storageBucket: "patchwork-4e0aa.firebasestorage.app",
  messagingSenderId: "24984571430",
  appId: "1:24984571430:web:daf756faf9cf6e559460e7",
  measurementId: "G-ESZ07VLK99",
};

let app;
let analytics;
let db;
let auth;

let unsubscribe = null;
let listenerStarted = false;
let resovleInitStateSaved;
let matchInProgress = false;
const initialStateSaved = new Promise((res) => {
  resovleInitStateSaved = res;
});

window.myUid = null;
let currentMatchId = null;
let current_role;

let authReady = false;
let hooksReady = false;

let lastTurn = 1;

let mpInitialized = false;

window.initializeMultiplayerMode = function (matchID, role) {
  console.log("multiplayer.js: called initializeMultiplayerMode");
  if (mpInitialized) return;
  mpInitialized = true;

  app = initializeApp(firebaseConfig);
  analytics = getAnalytics(app);
  db = getFirestore(app);
  auth = getAuth();

  onAuthStateChanged(auth, (user) => {
    if (!user) return;
    window.myUid = user.uid;
    authReady = true;
  });

  signInAnonymously(auth)
    .then(() => {})
    .catch((error) => {
      const errorCode = error.code;
      const errorMessage = error.message;
    });

  initMatchFromUrl(matchID, role);
};

function initMatchFromUrl(matchID, role) {
  currentMatchId = matchID;
  console.log("multiplayer.js: role: ", role);
  current_role = role;
  checkIfStateEmpty(currentMatchId).then(() => {
    maybeStart();
  });
}

async function maybeStart() {
  console.log("maybe start called");
  if (listenerStarted) return;
  console.log(authReady);
  console.log(hooksReady);
  console.log(currentMatchId);
  if (!authReady || !hooksReady || !currentMatchId) return;
  console.log("awaiting initial state saved");
  if (!matchInProgress) await initialStateSaved;
  console.log("initial state saved, start monitoring");
  monitorState(currentMatchId);
  listenerStarted = true;
  console.log("setting role: ", current_role);

  window.setRole?.(current_role);
  window.setUIReady();
}

Object.defineProperty(window, "getStateJson", {
  configurable: true,
  set(fn) {
    Object.defineProperty(window, "getStateJson", {
      value: fn,
      writable: true,
    });
    hooksReady = true;
  },
});

function monitorState(matchID) {
  console.log("called monitor state");
  if (unsubscribe) unsubscribe();
  unsubscribe = onSnapshot(doc(db, "matches", matchID), (doc) => {
    let ident = false;
    if (window.applyStateJson) {
      if (
        canonicalizeJSON(doc.data().stateJson) ===
        canonicalizeJSON(window.getStateJson())
      ) {
        ident = true;
        console.log(doc.data().stateJson);
        console.log("states are the same, skipping apply");
        return;
      }
      window.applyStateJson(
        doc.data().stateJson,
        doc.data().host,
        doc.data().joinee,
        false,
        current_role,
      );
      console.log("applied new state");
      console.log(doc.data().stateJson);
    } else console.log("applyStateJson not loaded yet");
  });
}

async function checkIfStateEmpty(matchID) {
  const docSnapshot = await getDoc(doc(db, "matches", matchID));
  if (!docSnapshot.exists()) {
    console.log("state empty, setting host");
    window.setHost(current_role);
  } else {
    matchInProgress = true;
    window.applyStateJson(
      docSnapshot.data().stateJson,
      docSnapshot.data().host,
      0,
      true,
      current_role,
    );
    console.log(docSnapshot.data().stateJson);
    console.log("state not empty, joining match in progress");
  }
}

async function markInitialStateSaved() {
  initialStateSaved = true;
}

async function saveState(json, h, j) {
  await setDoc(
    doc(db, "matches", currentMatchId),
    {
      stateJson: json,
      timestamp: serverTimestamp(),
      turn: lastTurn,
      host: h,
      joinee: j,
    },
    { merge: true },
  );
}
window.saveMPState = function (json, h, j) {
  if (!mpInitialized) return;
  let jsonState;
  if (!json) {
    jsonState = window.getStateJson();
    saveState(jsonState, h, j);
  } else {
    saveState(json, h, j);
  }
};

window.markInitialStateAsSaved = function () {
  resovleInitStateSaved && resovleInitStateSaved();
};

function canonicalizeJSON(s) {
  const sort = (v) => {
    if (Array.isArray(v)) return v.map(sort);
    if (v && typeof v === "object") {
      return Object.keys(v)
        .sort()
        .reduce((o, k) => ((o[k] = sort(v[k])), o), {});
    }
    return v;
  };
  return JSON.stringify(sort(JSON.parse(s)));
}

function generate_opId() {
  const a = crypto.getRandomValues(new Uint32Array(4));
  return [...a].map((x) => x.toString(16).padStart(8, "0")).join("-");
}
