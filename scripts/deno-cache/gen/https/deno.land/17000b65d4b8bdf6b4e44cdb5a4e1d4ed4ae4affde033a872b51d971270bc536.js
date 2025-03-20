// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.
// This module is browser compatible.
/**
 * Concatenate an array of byte slices into a single slice.
 *
 * @param buffers Array of byte slices to concatenate.
 * @returns Hello
 *
 * @example Basic usage
 * ```ts
 * import { concat } from "https://deno.land/std@$STD_VERSION/bytes/concat.ts";
 *
 * const a = new Uint8Array([0, 1, 2]);
 * const b = new Uint8Array([3, 4, 5]);
 *
 * concat([a, b]); // Uint8Array(6) [ 0, 1, 2, 3, 4, 5 ]
 * ```
 */ export function concat(buffers) {
  let length = 0;
  for (const buffer of buffers){
    length += buffer.length;
  }
  const output = new Uint8Array(length);
  let index = 0;
  for (const buffer of buffers){
    output.set(buffer, index);
    index += buffer.length;
  }
  return output;
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbImh0dHBzOi8vZGVuby5sYW5kL3N0ZEAwLjIyNC4wL2J5dGVzL2NvbmNhdC50cyJdLCJzb3VyY2VzQ29udGVudCI6WyIvLyBDb3B5cmlnaHQgMjAxOC0yMDI0IHRoZSBEZW5vIGF1dGhvcnMuIEFsbCByaWdodHMgcmVzZXJ2ZWQuIE1JVCBsaWNlbnNlLlxuLy8gVGhpcyBtb2R1bGUgaXMgYnJvd3NlciBjb21wYXRpYmxlLlxuXG4vKipcbiAqIENvbmNhdGVuYXRlIGFuIGFycmF5IG9mIGJ5dGUgc2xpY2VzIGludG8gYSBzaW5nbGUgc2xpY2UuXG4gKlxuICogQHBhcmFtIGJ1ZmZlcnMgQXJyYXkgb2YgYnl0ZSBzbGljZXMgdG8gY29uY2F0ZW5hdGUuXG4gKiBAcmV0dXJucyBIZWxsb1xuICpcbiAqIEBleGFtcGxlIEJhc2ljIHVzYWdlXG4gKiBgYGB0c1xuICogaW1wb3J0IHsgY29uY2F0IH0gZnJvbSBcImh0dHBzOi8vZGVuby5sYW5kL3N0ZEAkU1REX1ZFUlNJT04vYnl0ZXMvY29uY2F0LnRzXCI7XG4gKlxuICogY29uc3QgYSA9IG5ldyBVaW50OEFycmF5KFswLCAxLCAyXSk7XG4gKiBjb25zdCBiID0gbmV3IFVpbnQ4QXJyYXkoWzMsIDQsIDVdKTtcbiAqXG4gKiBjb25jYXQoW2EsIGJdKTsgLy8gVWludDhBcnJheSg2KSBbIDAsIDEsIDIsIDMsIDQsIDUgXVxuICogYGBgXG4gKi9cbmV4cG9ydCBmdW5jdGlvbiBjb25jYXQoYnVmZmVyczogVWludDhBcnJheVtdKTogVWludDhBcnJheSB7XG4gIGxldCBsZW5ndGggPSAwO1xuICBmb3IgKGNvbnN0IGJ1ZmZlciBvZiBidWZmZXJzKSB7XG4gICAgbGVuZ3RoICs9IGJ1ZmZlci5sZW5ndGg7XG4gIH1cbiAgY29uc3Qgb3V0cHV0ID0gbmV3IFVpbnQ4QXJyYXkobGVuZ3RoKTtcbiAgbGV0IGluZGV4ID0gMDtcbiAgZm9yIChjb25zdCBidWZmZXIgb2YgYnVmZmVycykge1xuICAgIG91dHB1dC5zZXQoYnVmZmVyLCBpbmRleCk7XG4gICAgaW5kZXggKz0gYnVmZmVyLmxlbmd0aDtcbiAgfVxuXG4gIHJldHVybiBvdXRwdXQ7XG59XG4iXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUEsMEVBQTBFO0FBQzFFLHFDQUFxQztBQUVyQzs7Ozs7Ozs7Ozs7Ozs7O0NBZUMsR0FDRCxPQUFPLFNBQVMsT0FBTyxPQUFxQjtFQUMxQyxJQUFJLFNBQVM7RUFDYixLQUFLLE1BQU0sVUFBVSxRQUFTO0lBQzVCLFVBQVUsT0FBTyxNQUFNO0VBQ3pCO0VBQ0EsTUFBTSxTQUFTLElBQUksV0FBVztFQUM5QixJQUFJLFFBQVE7RUFDWixLQUFLLE1BQU0sVUFBVSxRQUFTO0lBQzVCLE9BQU8sR0FBRyxDQUFDLFFBQVE7SUFDbkIsU0FBUyxPQUFPLE1BQU07RUFDeEI7RUFFQSxPQUFPO0FBQ1QifQ==
// denoCacheMetadata=2806256080268190974,17024952587781935016