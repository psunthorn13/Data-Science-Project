from flask import Flask,render_template,url_for,request
import pandas as pd
import pickle
from sklearn.linear_model import LogisticRegression
from sklearn.feature_extraction.text import CountVectorizer, TfidfVectorizer
from pythainlp import word_tokenize
from sklearn.metrics import classification_report

# Define flask app
app = Flask(__name__)

# load the model from disk
clf = pickle.load(open('tranform.pkl', 'rb'))
cv=pickle.load(open('nlp_model.pkl','rb'))

app = Flask(__name__)

@app.route("/")
def main():
    return render_template("home.html")


@app.route('/predict', methods=['POST'])
def predict():

    if request.method == 'POST':
        text = request.form['message']
        print(text)
        # data = [message]
        vect = clf.transform([text])
        print(vect)
        my_prediction = cv.predict(vect)
        my_prediction = my_prediction[0]

        email = my_prediction+'@mail.com'
        print(my_prediction)
    return render_template('result.html', prediction=my_prediction,message = text,email =email)


if __name__ == '__main__':
    app.run(debug=True)

