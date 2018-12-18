open Jest;

describe("DayTwo", () => {
  open Expect;
  test("letterFrequencies", () =>
    expect(DayTwo.letterFrequencies("bababc"))
    |> toEqual(Js.Dict.fromList([("b", 3), ("a", 2), ("c", 1)]))
  );

  test("checksum", () => {
    let puzzleInput = {|
      abcdef
      bababc
      abbcde
      abcccd
      aabcdd
      abcdee
      ababab
    |};

    expect(DayTwo.checksum(puzzleInput)) |> toEqual(12);
  });
});