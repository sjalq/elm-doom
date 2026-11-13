// node run.js WAD DEMO TICS
// Runs the compiled bench.js (elm make src/Bench.elm --optimize --output=bench.js)
// under node, giving elm/http a minimal XMLHttpRequest that reads local files.
const fs = require("fs");
const path = require("path");

class XMLHttpRequest {
  constructor() { this.listeners = {}; this.upload = { addEventListener() {} }; }
  open(method, url) { this.url = url; }
  setRequestHeader() {}
  addEventListener(name, f) { this.listeners[name] = f; }
  getAllResponseHeaders() { return ""; }
  send() {
    setTimeout(() => {
      try {
        const buf = fs.readFileSync(this.url);
        this.status = 200;
        this.statusText = "OK";
        this.responseURL = this.url;
        this.response = buf.buffer.slice(buf.byteOffset, buf.byteOffset + buf.byteLength);
        this.listeners.load && this.listeners.load();
      } catch (e) {
        this.listeners.error && this.listeners.error();
      }
    }, 0);
  }
}
global.XMLHttpRequest = XMLHttpRequest;

const [wad, demo, tics] = process.argv.slice(2);
const { Elm } = require(path.join(__dirname, "bench.js"));
const app = Elm.Bench.init({ flags: { wad, demo: demo || "DEMO1", tics: parseInt(tics || "1000", 10) } });
app.ports.report.subscribe((s) => { console.log(s); process.exit(0); });
