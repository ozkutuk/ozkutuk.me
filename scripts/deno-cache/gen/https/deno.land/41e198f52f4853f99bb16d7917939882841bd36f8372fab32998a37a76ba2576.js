// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.
// This module is browser compatible.
/**
 * Reader utility for combining multiple readers
 *
 * @deprecated This will be removed in 1.0.0. Use the {@link https://developer.mozilla.org/en-US/docs/Web/API/Streams_API | Web Streams API} instead.
 */ export class MultiReader {
  #readers;
  #currentIndex = 0;
  constructor(readers){
    this.#readers = [
      ...readers
    ];
  }
  async read(p) {
    const r = this.#readers[this.#currentIndex];
    if (!r) return null;
    const result = await r.read(p);
    if (result === null) {
      this.#currentIndex++;
      return 0;
    }
    return result;
  }
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbImh0dHBzOi8vZGVuby5sYW5kL3N0ZEAwLjIyNC4wL2lvL211bHRpX3JlYWRlci50cyJdLCJzb3VyY2VzQ29udGVudCI6WyIvLyBDb3B5cmlnaHQgMjAxOC0yMDI0IHRoZSBEZW5vIGF1dGhvcnMuIEFsbCByaWdodHMgcmVzZXJ2ZWQuIE1JVCBsaWNlbnNlLlxuLy8gVGhpcyBtb2R1bGUgaXMgYnJvd3NlciBjb21wYXRpYmxlLlxuXG5pbXBvcnQgdHlwZSB7IFJlYWRlciB9IGZyb20gXCIuL3R5cGVzLnRzXCI7XG5cbi8qKlxuICogUmVhZGVyIHV0aWxpdHkgZm9yIGNvbWJpbmluZyBtdWx0aXBsZSByZWFkZXJzXG4gKlxuICogQGRlcHJlY2F0ZWQgVGhpcyB3aWxsIGJlIHJlbW92ZWQgaW4gMS4wLjAuIFVzZSB0aGUge0BsaW5rIGh0dHBzOi8vZGV2ZWxvcGVyLm1vemlsbGEub3JnL2VuLVVTL2RvY3MvV2ViL0FQSS9TdHJlYW1zX0FQSSB8IFdlYiBTdHJlYW1zIEFQSX0gaW5zdGVhZC5cbiAqL1xuZXhwb3J0IGNsYXNzIE11bHRpUmVhZGVyIGltcGxlbWVudHMgUmVhZGVyIHtcbiAgcmVhZG9ubHkgI3JlYWRlcnM6IFJlYWRlcltdO1xuICAjY3VycmVudEluZGV4ID0gMDtcblxuICBjb25zdHJ1Y3RvcihyZWFkZXJzOiBSZWFkZXJbXSkge1xuICAgIHRoaXMuI3JlYWRlcnMgPSBbLi4ucmVhZGVyc107XG4gIH1cblxuICBhc3luYyByZWFkKHA6IFVpbnQ4QXJyYXkpOiBQcm9taXNlPG51bWJlciB8IG51bGw+IHtcbiAgICBjb25zdCByID0gdGhpcy4jcmVhZGVyc1t0aGlzLiNjdXJyZW50SW5kZXhdO1xuICAgIGlmICghcikgcmV0dXJuIG51bGw7XG4gICAgY29uc3QgcmVzdWx0ID0gYXdhaXQgci5yZWFkKHApO1xuICAgIGlmIChyZXN1bHQgPT09IG51bGwpIHtcbiAgICAgIHRoaXMuI2N1cnJlbnRJbmRleCsrO1xuICAgICAgcmV0dXJuIDA7XG4gICAgfVxuICAgIHJldHVybiByZXN1bHQ7XG4gIH1cbn1cbiJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQSwwRUFBMEU7QUFDMUUscUNBQXFDO0FBSXJDOzs7O0NBSUMsR0FDRCxPQUFPLE1BQU07RUFDRixDQUFBLE9BQVEsQ0FBVztFQUM1QixDQUFBLFlBQWEsR0FBRyxFQUFFO0VBRWxCLFlBQVksT0FBaUIsQ0FBRTtJQUM3QixJQUFJLENBQUMsQ0FBQSxPQUFRLEdBQUc7U0FBSTtLQUFRO0VBQzlCO0VBRUEsTUFBTSxLQUFLLENBQWEsRUFBMEI7SUFDaEQsTUFBTSxJQUFJLElBQUksQ0FBQyxDQUFBLE9BQVEsQ0FBQyxJQUFJLENBQUMsQ0FBQSxZQUFhLENBQUM7SUFDM0MsSUFBSSxDQUFDLEdBQUcsT0FBTztJQUNmLE1BQU0sU0FBUyxNQUFNLEVBQUUsSUFBSSxDQUFDO0lBQzVCLElBQUksV0FBVyxNQUFNO01BQ25CLElBQUksQ0FBQyxDQUFBLFlBQWE7TUFDbEIsT0FBTztJQUNUO0lBQ0EsT0FBTztFQUNUO0FBQ0YifQ==
// denoCacheMetadata=3913634269063161073,8451844278807483301