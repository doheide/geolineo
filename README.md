# geolineo

Server that generates a geojson representation optimized to visualize power grids. 
Lines are rendered to routes and the placement of the lines can be controlled.

![Ex1](docs/ex1.png)
![Ex2](docs/ex2.png)

# Input data
Format input data:

```json
{
  "locations": [ ...  ],
  "routes": [ ... ],
  "mapping": [ ... ],
  "unique_lines": [ ... ],
  "parameter": {
    "line_dist": 0.05,
    "kink_dist": 0.05
  },
  "connected_lines": [ ... ]
}
```

location data format:
```json
    {
      "guid": "guid_loc_A",
      "ll": { "lat": 0, "lng": 0 },
      "hidden": false,
      "properties": {
        "name": "A",
        "region": "reg_one"
      }
    }
```

routes data format:
```json
    {
      "guid": "guid_r_1",
      "loc_startend_mrid": ["guid_loc_B", "guid_loc_A"],
      "route_points":[{"lat": 0.5, "lng": 1.0 }, {"lat": 1.0, "lng": 0.5}, {"lat": 0.0, "lng": 0.0}]
    }
```

...

# Render 

...