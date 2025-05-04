# PPA-Final
## Setup
Create python venv:
```
python3 -m venv .venv
```

Activate venv:
```
. ./.venv/bin/activate
```

Install requirements:
```
pip install numpy libigl scikit-learn ipywidgets matplotlib pythreejs
```

Install `meshplot`:
```
pip install git+https://github.com/skoch9/meshplot.git
```

Add `mpl-v053/bin` to your path.
And also setup your ipykernel...


Build files in `sml-lib` using `mpl`.