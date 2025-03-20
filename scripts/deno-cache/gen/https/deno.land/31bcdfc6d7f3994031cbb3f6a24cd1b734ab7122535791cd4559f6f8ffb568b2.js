// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.
// This module is browser compatible.
/**
 * Copy bytes from the source array to the destination array and returns the
 * number of bytes copied.
 *
 * If the source array is larger than what the `dst` array can hold, only the
 * amount of bytes that fit in the `dst` array are copied.
 *
 * @param src Source array to copy from.
 * @param dst Destination array to copy to.
 * @param offset Offset in the destination array to start copying to. Defaults
 * to 0.
 * @returns Number of bytes copied.
 *
 * @example Basic usage
 * ```ts
 * import { copy } from "https://deno.land/std@$STD_VERSION/bytes/copy.ts";
 *
 * const src = new Uint8Array([9, 8, 7]);
 * const dst = new Uint8Array([0, 1, 2, 3, 4, 5]);
 *
 * copy(src, dst); // 3
 * dst; // Uint8Array(6) [9, 8, 7, 3, 4, 5]
 * ```
 *
 * @example Copy with offset
 * ```ts
 * import { copy } from "https://deno.land/std@$STD_VERSION/bytes/copy.ts";
 *
 * const src = new Uint8Array([1, 1, 1, 1]);
 * const dst = new Uint8Array([0, 0, 0, 0]);
 *
 * copy(src, dst, 1); // 3
 * dst; // Uint8Array(4) [0, 1, 1, 1]
 * ```
 * Defining an offset will start copying at the specified index in the
 * destination array.
 */ export function copy(src, dst, offset = 0) {
  offset = Math.max(0, Math.min(offset, dst.byteLength));
  const dstBytesAvailable = dst.byteLength - offset;
  if (src.byteLength > dstBytesAvailable) {
    src = src.subarray(0, dstBytesAvailable);
  }
  dst.set(src, offset);
  return src.byteLength;
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbImh0dHBzOi8vZGVuby5sYW5kL3N0ZEAwLjIyNC4wL2J5dGVzL2NvcHkudHMiXSwic291cmNlc0NvbnRlbnQiOlsiLy8gQ29weXJpZ2h0IDIwMTgtMjAyNCB0aGUgRGVubyBhdXRob3JzLiBBbGwgcmlnaHRzIHJlc2VydmVkLiBNSVQgbGljZW5zZS5cbi8vIFRoaXMgbW9kdWxlIGlzIGJyb3dzZXIgY29tcGF0aWJsZS5cblxuLyoqXG4gKiBDb3B5IGJ5dGVzIGZyb20gdGhlIHNvdXJjZSBhcnJheSB0byB0aGUgZGVzdGluYXRpb24gYXJyYXkgYW5kIHJldHVybnMgdGhlXG4gKiBudW1iZXIgb2YgYnl0ZXMgY29waWVkLlxuICpcbiAqIElmIHRoZSBzb3VyY2UgYXJyYXkgaXMgbGFyZ2VyIHRoYW4gd2hhdCB0aGUgYGRzdGAgYXJyYXkgY2FuIGhvbGQsIG9ubHkgdGhlXG4gKiBhbW91bnQgb2YgYnl0ZXMgdGhhdCBmaXQgaW4gdGhlIGBkc3RgIGFycmF5IGFyZSBjb3BpZWQuXG4gKlxuICogQHBhcmFtIHNyYyBTb3VyY2UgYXJyYXkgdG8gY29weSBmcm9tLlxuICogQHBhcmFtIGRzdCBEZXN0aW5hdGlvbiBhcnJheSB0byBjb3B5IHRvLlxuICogQHBhcmFtIG9mZnNldCBPZmZzZXQgaW4gdGhlIGRlc3RpbmF0aW9uIGFycmF5IHRvIHN0YXJ0IGNvcHlpbmcgdG8uIERlZmF1bHRzXG4gKiB0byAwLlxuICogQHJldHVybnMgTnVtYmVyIG9mIGJ5dGVzIGNvcGllZC5cbiAqXG4gKiBAZXhhbXBsZSBCYXNpYyB1c2FnZVxuICogYGBgdHNcbiAqIGltcG9ydCB7IGNvcHkgfSBmcm9tIFwiaHR0cHM6Ly9kZW5vLmxhbmQvc3RkQCRTVERfVkVSU0lPTi9ieXRlcy9jb3B5LnRzXCI7XG4gKlxuICogY29uc3Qgc3JjID0gbmV3IFVpbnQ4QXJyYXkoWzksIDgsIDddKTtcbiAqIGNvbnN0IGRzdCA9IG5ldyBVaW50OEFycmF5KFswLCAxLCAyLCAzLCA0LCA1XSk7XG4gKlxuICogY29weShzcmMsIGRzdCk7IC8vIDNcbiAqIGRzdDsgLy8gVWludDhBcnJheSg2KSBbOSwgOCwgNywgMywgNCwgNV1cbiAqIGBgYFxuICpcbiAqIEBleGFtcGxlIENvcHkgd2l0aCBvZmZzZXRcbiAqIGBgYHRzXG4gKiBpbXBvcnQgeyBjb3B5IH0gZnJvbSBcImh0dHBzOi8vZGVuby5sYW5kL3N0ZEAkU1REX1ZFUlNJT04vYnl0ZXMvY29weS50c1wiO1xuICpcbiAqIGNvbnN0IHNyYyA9IG5ldyBVaW50OEFycmF5KFsxLCAxLCAxLCAxXSk7XG4gKiBjb25zdCBkc3QgPSBuZXcgVWludDhBcnJheShbMCwgMCwgMCwgMF0pO1xuICpcbiAqIGNvcHkoc3JjLCBkc3QsIDEpOyAvLyAzXG4gKiBkc3Q7IC8vIFVpbnQ4QXJyYXkoNCkgWzAsIDEsIDEsIDFdXG4gKiBgYGBcbiAqIERlZmluaW5nIGFuIG9mZnNldCB3aWxsIHN0YXJ0IGNvcHlpbmcgYXQgdGhlIHNwZWNpZmllZCBpbmRleCBpbiB0aGVcbiAqIGRlc3RpbmF0aW9uIGFycmF5LlxuICovXG5leHBvcnQgZnVuY3Rpb24gY29weShzcmM6IFVpbnQ4QXJyYXksIGRzdDogVWludDhBcnJheSwgb2Zmc2V0ID0gMCk6IG51bWJlciB7XG4gIG9mZnNldCA9IE1hdGgubWF4KDAsIE1hdGgubWluKG9mZnNldCwgZHN0LmJ5dGVMZW5ndGgpKTtcbiAgY29uc3QgZHN0Qnl0ZXNBdmFpbGFibGUgPSBkc3QuYnl0ZUxlbmd0aCAtIG9mZnNldDtcbiAgaWYgKHNyYy5ieXRlTGVuZ3RoID4gZHN0Qnl0ZXNBdmFpbGFibGUpIHtcbiAgICBzcmMgPSBzcmMuc3ViYXJyYXkoMCwgZHN0Qnl0ZXNBdmFpbGFibGUpO1xuICB9XG4gIGRzdC5zZXQoc3JjLCBvZmZzZXQpO1xuICByZXR1cm4gc3JjLmJ5dGVMZW5ndGg7XG59XG4iXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUEsMEVBQTBFO0FBQzFFLHFDQUFxQztBQUVyQzs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7O0NBb0NDLEdBQ0QsT0FBTyxTQUFTLEtBQUssR0FBZSxFQUFFLEdBQWUsRUFBRSxTQUFTLENBQUM7RUFDL0QsU0FBUyxLQUFLLEdBQUcsQ0FBQyxHQUFHLEtBQUssR0FBRyxDQUFDLFFBQVEsSUFBSSxVQUFVO0VBQ3BELE1BQU0sb0JBQW9CLElBQUksVUFBVSxHQUFHO0VBQzNDLElBQUksSUFBSSxVQUFVLEdBQUcsbUJBQW1CO0lBQ3RDLE1BQU0sSUFBSSxRQUFRLENBQUMsR0FBRztFQUN4QjtFQUNBLElBQUksR0FBRyxDQUFDLEtBQUs7RUFDYixPQUFPLElBQUksVUFBVTtBQUN2QiJ9
// denoCacheMetadata=10631233208593495270,5088297346005112353