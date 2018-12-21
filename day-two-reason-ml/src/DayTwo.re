let incrementOrSetFrequency =
    (frequencies: Js.Dict.t(int), letter: string): Js.Dict.t(int) => {
  switch (Js.Dict.get(frequencies, letter)) {
  | Some(x) =>
    Js.Dict.set(frequencies, letter, x + 1);
    frequencies;
  | None =>
    Js.Dict.set(frequencies, letter, 1);
    frequencies;
  };
};

let letterFrequencies = (input: string): Js.Dict.t(int) => {
  let frequencies = Js.Dict.empty();
  input
  |> Js.String.split("")
  |> Js.Array.reduce(
       (acc, currentValue) => incrementOrSetFrequency(acc, currentValue),
       frequencies,
     );
};

let hasTwices = (frequencies: Js.Dict.t(int)): bool => {
  frequencies |> Js.Dict.values |> Js.Array.some(v => v === 2);
};

let hasThrices = (frequencies: Js.Dict.t(int)): bool => {
  frequencies |> Js.Dict.values |> Js.Array.some(v => v === 3);
};

type twiceAndThriceFrequency = {
  twice: bool,
  thrice: bool,
};

type twiceAndThriceCounter = {
  twice: int,
  thrice: int,
};

let findTwicesAndThrices =
    (frequencies: Js.Dict.t(int)): twiceAndThriceFrequency => {
  {twice: hasTwices(frequencies), thrice: hasThrices(frequencies)};
};

let checksum = (input: string): int => {
  input
  |> Js.String.split("\n")
  |> Js.Array.map(String.trim)
  |> Js.Array.filter(s => String.length(s) > 0)
  |> Js.Array.map(letterFrequencies)
  |> Js.Array.map(findTwicesAndThrices)
  |> Js.Array.reduce(
       (acc: twiceAndThriceCounter, currentValue: twiceAndThriceFrequency) =>
         switch (currentValue) {
         | {twice: true, thrice: true} => {
             twice: acc.twice + 1,
             thrice: acc.thrice + 1,
           }
         | {twice: true, thrice: false} => {
             twice: acc.twice + 1,
             thrice: acc.thrice,
           }
         | {twice: false, thrice: true} => {
             twice: acc.twice,
             thrice: acc.thrice + 1,
           }
         | {twice: false, thrice: false} => acc
         },
       {twice: 0, thrice: 0},
     )
  |> (({twice, thrice}) => twice * thrice);
};

let puzzleInput = {|
tjxmoewpqkyaiqvmndgflunszc
tjxmobwpqkyaihvrndgfjubszm
tjxmzewpqkyaihvrydgflrbszc
tjxmoeypqkyvihvrndgflubsza
tjcmoewpqkytihvrndgflgbszc
tjvmoewpqkyanevrndgflubszc
tjxmoewpqkdiihirndgflubszc
tjxboewpqkyaihbrnogflubszc
ojpmoewpqkyaihvjndgflubszc
tjxyoewpqkyaiuvrndgflutszc
tjxmoewpqkyalhvrndmflebszc
tjxmoewpqzyaihhrndgflubszf
tjxmrewpqkyaihirndgfjubszc
pjxmoewpqkyaihvendgfbubszc
txxmkewpqkyjihvrndgflubszc
tjxmoewcqkyaihvrnmgflubczc
tjxmoewkqkyaghvrndgfluvszc
tjxmoewfqkhaihvrndgflubhzc
jjqmoewpqkyaihvrndzflubszc
tjxmoewmqksaihvcndgflubszc
tjrmoewpqkyaihvrvdgflubzzc
tjxxoewpqkyaiiwrndgflubszc
cjxmoawxqkyaihvrndgflubszc
tjxdoewpvkyaihvrndgflubsoc
tjxmsewpqkyaihvrndgfluzozc
tjxmoewpqkyafhvrnyeflubszc
tjxmlewpqkyawhvondgflubszc
tjxmonwpqkyaiqvrnxgflubszc
tjxmoewcqkyaihvrnjgflumszc
tjvmoewpqkyaihveadgflubszc
tjxmogfpqkyaigvrndgflubszc
tybmoewpqkyaihvrndgllubszc
tjxmoewpdkyaihvrndgfluwbzc
etxmbewpqkyaihvrndgflubszc
tjxmoeapqcynihvrndgflubszc
tbxmoewpqkyaihvrndgfdebszc
haxmoewpqyyaihvrndgflubszc
ojxmoewpqkyaihvrnegflubszr
tjxmoewpqkyaihvrndoflubarc
ljxmoewpqkykihvrndgflvbszc
tjxmovwpqkyaihvrndgfluzsyc
tvxmoewpqkyanhvrkdgflubszc
tjxmoewpqkyaihkrndgfluwwzc
zjxmoewpfkyaihvrndgfrubszc
tjxyoegpqkyaihvrndlflubszc
tjxmoewpqkyamhvrnsgflubmzc
tjmmoewpqkyaihvrndgftuwszc
tjxmoewpqbraihvrncgflubszc
tjxmeeepqkyainvrndgflubszc
tjemoegpqkyaihvredgflubszc
tjxmoewpqkyaihvdndgfzubqzc
tjxmoegrqkyaihfrndgflubszc
tjxmoewpqxyaihvrndgfluyvzc
qjxmoewpqkyaiwvrnfgflubszc
tjxwoewpqkyashkrndgflubszc
tjzmoewiqkyaihurndgflubszc
tjumuewpqkyaihvrndgflubssc
tyxooewpukyaihvrndgflubszc
tjxvoewpqkyaiivindgflubszc
ijxmoqwpqkyaihvradgflubszc
tjxmlewpqkyaihvrhdgflubwzc
tjxmkewpqkyajhqrndgflubszc
tjxmoewpqkqaiherndgflurszc
tjamoewpqkyaizvondgflubszc
tjxgogwpqkyalhvrndgflubszc
tjxmoewpqkyachvrndgflubuzq
tjxmowqpqkyaihvrnegflubszc
mjxmoewpwkyaihvrndgfkubszc
tpbmoewpqkyaihvrzdgflubszc
tjbmoewpqkyaiuvrndgflsbszc
tjxmoewpqklaghvrndgflubazc
tjxmoewpqkyrihvrndgwlpbszc
tjcmoewpqksaiyvrndgflubszc
tjxmoeapqkymihvindgflubszc
tjxmdewpqkyafhvrndgflqbszc
tjxmoewpqxyaihvrndsflubszi
tjxmoeppqkyaihvrcdgflubszd
tjxmomwpqkyainvrmdgflubszc
tjxmovwpqkyaihvrndgfdubdzc
tjxmoewwqkiaihvrjdgflubszc
tmxmoewpqkyaifvrndgflubszs
tbxmoewpqkyaihvrbdgflunszc
tjxmoewrqkyaihvxndgflubszp
ujxmoewpqkyaihvxndgflubpzc
tdxmotwpqkyaihvdndgflubszc
tjxmvewpqkyaihfrndgtlubszc
tjfmoewpqkyaihvrnyqflubszc
tjxfolwzqkyaihvrndgflubszc
ojrmoiwpqkyaihvrndgflubszc
tjsmoqwpqkyqihvrndgflubszc
tjxmohwpqkyaihvrudgflubslc
tjxtoiwpqkyaihvrnogflubszc
taxmoewpqkyaiyvrndgfwubszc
tjxwnezpqkyaihvrndgflubszc
tjxmyevpqkyaivvrndgflubszc
tjxdoeopqkyaihvgndgflubszc
tjxaoewpqkmaihvrndgflufszc
tjxmoewpqkyaxhvrndgflubncc
tjxmoewpqkyaihurndgflubbjc
tjxmjewpqgyaihvrnngflubszc
tjxmogwpqkyaihvrndgflubbcc
tjxmoewplkyaihvrnpgflibszc
tjwmoewpqkyaohvrndgfbubszc
tjwmoewpqkyaihvrndgfsubszm
tjxmogwpqkyaihvrndiflubqzc
tjxmoewpqkyaihvrndgflopshc
rjxmlewpvkyaihvrndgflubszc
tjxmogwpakyaihvrndgflzbszc
tjxmoeppqkyaihvrndgflmxszc
tjxmoewpqkyhihgrndgfzubszc
tjxqoewpqkyaihtrndgwlubszc
tjxnoespqkyaihvrndgflubsuc
tjmmoewpqkraihvrndgflfbszc
tjxmoewnqkwaihvrndgflubstc
tjxmoewpqqyaihvrndgfljbszi
tjxmoewpqkyaihkrkdgalubszc
tjxmoewpqkyaihvradgjlurszc
tvxmoewpqkybihvrndbflubszc
tjxvoewpqkyaihvradgfoubszc
tjxmoewpqfyaihvlodgflubszc
tjxmoewmnkyaiivrndgflubszc
kjxmoewpqkyaihprndgflcbszc
hjxmoewpqkcaihvrndgvlubszc
tjxmoewcqkyaihvrncgfllbszc
tuxmoewpckyaihvrndoflubszc
tjxmdewpokyaihvrndgflubszn
mjxmaewpqkyaqhvrndgflubszc
tjxmoewpmzyaihvrndgfiubszc
tjxmoewnqkyvihvrndgflubszk
tjxmoewpmnyaihvrndgftubszc
zjxmoewpqkysihvrndgfmubszc
tjxmoewpqkyaihzrntgflubbzc
tjxmoewpqkgaihwrndsflubszc
tjxjoewpqkyaihvrndgflgbizc
oqxmoewpqkyaihvrndgfldbszc
wjamoewpqkyaihvfndgflubszc
tjxmoewtmkyvihvrndgflubszc
tjlmojwpqkyaihvrndgfludszc
tjxmowwpqkyaihvrndefludszc
tjxmoewpqkbaihvrndgfluaszt
tjxmoewpqkzaahvrodgflubszc
tjxmoewpqkgaihvrndgflubtpc
tjxmoenpqkyaihcrndgfqubszc
tbxmoewpqbyaihvrndgalubszc
tjvmoewqqkyaihvrndvflubszc
tjxmoewpqkeaihvundgfaubszc
txxmoewpqkyaihvrwdgflpbszc
tzxmoewpqkijihvrndgflubszc
tjxmoewoqkytiuvrndgflubszc
tjxmrejplkyaihvrndgflubszc
tjxmoewpqkynihvrpxgflubszc
tjxmoewpqkvanhvrndgvlubszc
tjxmoewpdkyiihvrndgflubszs
tpxmyewpqkyaihvrndgfeubszc
tpxmoewpqyyaihvrndhflubszc
tjsmoewpqkyaihvrndhflubnzc
tjxmoewpukyaihvrnmgflubwzc
txxmoewpqlyaihwrndgflubszc
tjxmoewprkyaiovrndgflubxzc
tjxmouwpqkyaihzrodgflubszc
tjxmojwpqkywimvrndgflubszc
tjxsoewpqkyaihvrzdgqlubszc
tfxmoewpakyaihvrndgllubszc
tjhmoewpqiyaihvrndgflubsac
tjxmoewpqkoaihvrndoflubsxc
tjxmoewpqkyzpjvrndgflubszc
tjxmoewpqkyaiharndgzlnbszc
tjimoevpqkyaihvrndgflubbzc
tjxsoewpqkyahhvrndgfzubszc
txxmoewpqkyaimvrrdgflubszc
tjxmoewpwkyaihvrndpylubszc
tjxmoewpskyaghvrndgfbubszc
tjxmuewpqmyaihvrndgfyubszc
tjxmoewpqkyaihvdndgglubsxc
xjxmoewpqkyjiovrndgflubszc
gjxmoewpqkyaihvrndodlubszc
tjbmoewpqkyaihvridgflvbszc
tjxmozwpqkyapbvrndgflubszc
tjxeoewpqkyqihvrndgflubhzc
tjxdoewpqzyaihvrndgflubsmc
tjxmwewpqkyathvcndgflubszc
tjxmoewpszyaihvrndgflusszc
tuxmoewpqkyaihvrndgfluasxc
tjemoewpnvyaihvrndgflubszc
tjxmoewpjkyaihvrndgjlufszc
tjomoewppkyaihvzndgflubszc
tjxmvewpqkyaimvrntgflubszc
rjxmoewpqkyaihvpndgflubszq
hjxzoewpqkyaihvridgflubszc
texmoejpqkyaihvrndgflubszx
tjxcoewpqkyaihbrxdgflubszc
tjxmoewpnkyaihvrndgfltbsze
tjxmoewpdkyaihvrndwfluwbzc
tjxmoewpqryjihkrndgflubszc
tjlmoewpqkhaihvrndgflubsnc
tjxmovapqkjaihvrndgflubszc
tjxvoewpqkyaihqrndgfluyszc
tjxwoewnqkyaihvrndgfgubszc
tjdmoewpqklaihvcndgflubszc
tjxmoewpvkynihvrndgflubskc
tjxmtewpqkyaihvhndgfluaszc
tjxmoewpqkyanhvrnpgfluvszc
tjxmoewpqkyaifvbndgflubspc
tjxmoexpqknaihvrndgxlubszc
qjxmoewqqkyaihvrndgflubpzc
tjxmoewppkyaihvaxdgflubszc
myxmoewpqkyaihvrudgflubszc
tjxmwewpmkyaihvrndgflubssc
tjxmoewpqkyaihvrndgfxqbszq
tjxmoewhqkyaahvrndgflubbzc
tbxmoewmqkyaihvrndgflubszu
toxmolwpqkyaihvrndnflubszc
tjxmoewhqkyaihvrnrgflubvzc
yjxmoewcqkyaihvrndgflubfzc
tjxmoewpqkyamhvrgdgflmbszc
tjxmtewpqkyaizvrndgfluoszc
tjxmoewpqzyaihvrntsflubszc
fjxmoewpqkyaihyrmdgflubszc
tjxwoewpqcyaihbrndgflubszc
tjxmoebpqkzaihvrndcflubszc
tjxmoewpqkyaihvrndnlmubszc
tjxmoewpqkyaihvrndgeyubskc
tfxmoewpqryaihvrndgfluiszc
tjxmoewpqkjaihvynngflubszc
tjxmoewpqkqaihvonjgflubszc
tjfmokwpqkyeihvrndgflubszc
djxmoewpkkyaihvrnmgflubszc
tjxmiewpqkyaihvrndgflubhlc
tjxmmejpqkyaihvrnhgflubszc
djxmoewmqkyaihvrnggflubszc
tjxmoewpqkyaihvrkggflubsze
gjxmoewpqkyaihjrndgflvbszc
tjxmoewpqkyaidvrndgkzubszc
tjxmoewpqkyaedvrnpgflubszc
sjxmoewpqkyaihvrnngfluhszc
tjxmoewpqkuaihvrndghlubxzc
tjxmoewgqkyuihvrndgftubszc
tjxmoewpqsyaifvrkdgflubszc
tjxrrewpqkyaihvrnpgflubszc
tjxmoezpqkyaihvrwdgflabszc
tjfmoewpqknaihvrndgflubkzc
tjxmoewnqkxaihvrndgflubtzc
tjxmoewpkkyaihvrndgfrnbszc
tjxmorwpnkqaihvrndgflubszc
tsxmoewwqkyathvrndgflubszc
tjxmoeupqkyaihvrndyflubszp
bjxmoewdqkyaihvrndgflurszc
tjxmoewpvkyaihvrndoflubszq
sjxmoewpqkyaihvrndgflubyec
tjxmoewpqkyaizcrndgfnubszc
|};

Js.log(puzzleInput->checksum);