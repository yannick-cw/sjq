# sjq

## About

Use Scala syntax to modify json fast and however you want from the Commandline.

Just pass `code` to modify the `root` case class. sjq parses the json to a case class and allows editing and modifying it in any way.

e.g.

```bash
sjq -a 'root.subclass.copy(name = root.subclass.name + "Jo")' -j '{ "subclass": { "name": "Ho", "ids": [22, 23, 24]  }}'
{
  "name" : "HoJo",
  "ids" : [
    22.0,
    23.0,
    24.0
  ]
}
```

One example with a more complex json and a remote api:

##### Get all hotel names with a score over 300

```
curl https://www.holidaycheck.de/svc/search-api/search/mall\?tenant\=test \
| sjq -a 'root.destinations.entities.filter(_.rankingScore > 300).map(_.name)'
```

returns

```
[
  "Mallorca",
  "Malles Venosta / Mals",
  "Palma de Mallorca"
]
```

## Install 

`brew install yannick-cw/homebrew-tap/sjq`

Or download the executable from the releases.

## Usage

You can pass any valid scala code to access the json, the json is internally represented as a case class.

The input to use is the case class `root`

```bash
sjq -a 'root.subclass.ids.filter(id => id > 22)' -j '{ "subclass": { "name": "Ho", "ids": [22, 23, 24]  }}'
## Results in 
[
  23.0,
  24.0
]
```

### Alternatively pipe input

```bash
echo '{ "subclass": { "name": "Ho", "ids": [22, 23, 24]  }}' | sjq -a 'root.subclass.ids.filter(id => id > 22)'
```
