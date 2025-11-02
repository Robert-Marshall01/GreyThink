import importlib.metadata as md
import json
pkgs=['numpy','matplotlib','pytest']
out={}
for p in pkgs:
    try:
        m=md.metadata(p)
        out[p]={'License':m.get('License'), 'Classifiers':m.get_all('Classifier')}
    except Exception as e:
        out[p]={'error':str(e)}
print(json.dumps(out, indent=2))
