import paho.mqtt.client as mqtt

import time

broker = "broker.hivemq.com"
broker = "iot.eclipse.org"

MQTT_TOPIC = [("LightStatus",2),("Status/RaspberryPiA",2),("Status/RaspberryPiC",2),("threshold",2),("lightSensor",2)]
MQTT_BROKER = "10.97.143.44"


def on_connect(client, userdata, flags, rc):
	print("connected with broker"+str(rc))
	client.subscribe(MQTT_TOPIC)



def on_message(client, userdata, msg):
	if msg.topic == "LightStatus":
		print("message response: ",str(message.payload.decode()))
		print(time.time())
	if msg.topic == "Status/RaspberryPiA":
		print("message response: ",str(message.payload.decode()))
		print(time.time())
	if msg.topic == "Status/RaspberryPiC":
		print("message response: ",str(message.payload.decode()))
		print(time.time())
	if msg.topic == "threshold":
		print("message response: ",str(message.payload.decode()))
		print(time.time())
	if msg.topic == "lightSensor":
		print("message response: ",str(message.payload.decode()))
		print(time.time())
	
	time.sleep(1)
	

client = mqtt.Client()


client.on_connect = on_connect
client.on_message = on_message

client.connect(MQTT_BROKER, 1883, 60)



client.loop_forever()
