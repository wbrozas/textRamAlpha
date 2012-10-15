from twilio.rest import TwilioRestClient
from flask import Flask, render_template, request
import commands

app = Flask(__name__)

@app.route('/', methods=['GET', 'POST'])
def home():
	argfrom = "Nothing"
	argbody = "Nothing"
	if 'From' in request.args:
		argfrom = request.args['From']
	if 'Body' in request.args:
		argbody = request.args['Body']
	output = commands.getoutput('/home/wbrozas/textRamAlpha "' + argbody  + '"')
	print 'From:' + argfrom + ' Body:' + argbody + ' Output:' + output

	account = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" 
	token = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
	client = TwilioRestClient(account, token)

	message = client.sms.messages.create(to=argfrom, from_="+1XXXXXXXXXX", body=output)

	return 'From:' + argfrom + ' Body:' + argbody + ' Output:' + output

if __name__ == '__main__':
	app.run('0.0.0.0', debug=True) 
