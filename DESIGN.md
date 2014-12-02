
# HTTP API

## Endpoint Online

### Request

```
HTTP POST '/endpoints/online'
```

### Response

```
{
	status: "ok",
	data: {
		ticket: "", 
		server: {
			jsonp: "", 
			wsocket: "", 
			mqtt: ""
		}
	}
}
```

## Endpoint Offline

### Request

```
HTTP POST '/endpoints/offline'
```

### Response

```
{
	status: "ok"
}
```

## Publish Presence

### Request

```
HTTP POST '/presences'
```

### Response

```
{
	status: "ok"
}
```

## Get Presences

### Request

```
HTTP GET '/presences'
```

### Response

```
{
	uid1: "available",
	uid2: "unavailable"
}
```

## Send Message

### Request

```
HTTP POST '/messages'
```

with ticket....

### Response

```
{
	status: "ok"
}
```

## Push Message

## Request

```
HTTP POST '/messages'
```
with from...,no ticket

### Response

```
{status: "ok"}
```

## Join Room

### Request

```
HTTP POST '/rooms/:id/join'
```

### Response

```
{status: "ok"}
```

## Leave Room

### Request

```
HTTP POST '/rooms/:id/leave'
```

### Response

```
{status: "ok"}
```

## Get Room Members

### Request

```
HTTP POST '/rooms/:id/memebers'
```
### Response

```
{
	uid1: "available",
	uid2" "away"
}
```

## Long Poll Packets

### Request

```
HTTP GET '/packets'
```

### Response

```
{status: "ok", data: []}
```

## WebSocket Push

### URL

```
/wsocket
```




