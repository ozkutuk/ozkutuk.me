// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.
// This module is browser compatible.
/**
 * Slice number into 64bit big endian byte array
 * @param d The number to be sliced
 * @param dest The sliced array
 *
 * @deprecated This will be removed in 1.0.0. Use the {@link https://developer.mozilla.org/en-US/docs/Web/API/Streams_API | Web Streams API} instead.
 */ export function sliceLongToBytes(d, dest = Array.from({
  length: 8
})) {
  let big = BigInt(d);
  for(let i = 0; i < 8; i++){
    dest[7 - i] = Number(big & 0xffn);
    big >>= 8n;
  }
  return dest;
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbImh0dHBzOi8vZGVuby5sYW5kL3N0ZEAwLjIyNC4wL2lvL3NsaWNlX2xvbmdfdG9fYnl0ZXMudHMiXSwic291cmNlc0NvbnRlbnQiOlsiLy8gQ29weXJpZ2h0IDIwMTgtMjAyNCB0aGUgRGVubyBhdXRob3JzLiBBbGwgcmlnaHRzIHJlc2VydmVkLiBNSVQgbGljZW5zZS5cbi8vIFRoaXMgbW9kdWxlIGlzIGJyb3dzZXIgY29tcGF0aWJsZS5cblxuLyoqXG4gKiBTbGljZSBudW1iZXIgaW50byA2NGJpdCBiaWcgZW5kaWFuIGJ5dGUgYXJyYXlcbiAqIEBwYXJhbSBkIFRoZSBudW1iZXIgdG8gYmUgc2xpY2VkXG4gKiBAcGFyYW0gZGVzdCBUaGUgc2xpY2VkIGFycmF5XG4gKlxuICogQGRlcHJlY2F0ZWQgVGhpcyB3aWxsIGJlIHJlbW92ZWQgaW4gMS4wLjAuIFVzZSB0aGUge0BsaW5rIGh0dHBzOi8vZGV2ZWxvcGVyLm1vemlsbGEub3JnL2VuLVVTL2RvY3MvV2ViL0FQSS9TdHJlYW1zX0FQSSB8IFdlYiBTdHJlYW1zIEFQSX0gaW5zdGVhZC5cbiAqL1xuZXhwb3J0IGZ1bmN0aW9uIHNsaWNlTG9uZ1RvQnl0ZXMoXG4gIGQ6IG51bWJlcixcbiAgZGVzdDogbnVtYmVyW10gPSBBcnJheS5mcm9tPG51bWJlcj4oeyBsZW5ndGg6IDggfSksXG4pOiBudW1iZXJbXSB7XG4gIGxldCBiaWcgPSBCaWdJbnQoZCk7XG4gIGZvciAobGV0IGkgPSAwOyBpIDwgODsgaSsrKSB7XG4gICAgZGVzdFs3IC0gaV0gPSBOdW1iZXIoYmlnICYgMHhmZm4pO1xuICAgIGJpZyA+Pj0gOG47XG4gIH1cbiAgcmV0dXJuIGRlc3Q7XG59XG4iXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUEsMEVBQTBFO0FBQzFFLHFDQUFxQztBQUVyQzs7Ozs7O0NBTUMsR0FDRCxPQUFPLFNBQVMsaUJBQ2QsQ0FBUyxFQUNULE9BQWlCLE1BQU0sSUFBSSxDQUFTO0VBQUUsUUFBUTtBQUFFLEVBQUU7RUFFbEQsSUFBSSxNQUFNLE9BQU87RUFDakIsSUFBSyxJQUFJLElBQUksR0FBRyxJQUFJLEdBQUcsSUFBSztJQUMxQixJQUFJLENBQUMsSUFBSSxFQUFFLEdBQUcsT0FBTyxNQUFNLEtBQUs7SUFDaEMsUUFBUSxFQUFFO0VBQ1o7RUFDQSxPQUFPO0FBQ1QifQ==
// denoCacheMetadata=4925728070148163353,10839815062662869733