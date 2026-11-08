const testlib = require("../testlib.js");
const os = require("os"); // not using 'node:os' so that it still works on Node.js 15

testlib.printint(testlib.add32(13, 29));
testlib.printint(os.totalmem() > (1024 * 1024) ? 1 : 0);
