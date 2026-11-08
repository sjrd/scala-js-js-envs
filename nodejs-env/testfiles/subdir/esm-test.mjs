import { add32, printint } from '../testlib.js';
import { totalmem } from 'node:os';

printint(add32(13, 29));
printint(totalmem() > (1024 * 1024) ? 1 : 0);
