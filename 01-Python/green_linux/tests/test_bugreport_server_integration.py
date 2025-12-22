import json
import threading
import http.server
import socket
import time

from powerapp.system import bugreport as br


class _SilentHandler(http.server.BaseHTTPRequestHandler):
    server_state = {}

    def do_POST(self):
        length = int(self.headers.get('Content-Length', 0))
        body = self.rfile.read(length) if length else b''
        try:
            data = json.loads(body.decode('utf-8'))
        except Exception:
            data = {'_raw': body.decode('utf-8')}
        # record last post
        type(self).server_state['last'] = {'path': self.path, 'data': data, 'headers': dict(self.headers)}
        resp = {'id': 'srv123', 'url': f'http://{self.server.server_address[0]}:{self.server.server_address[1]}/report/srv123'}
        resp_b = json.dumps(resp).encode('utf-8')
        self.send_response(200)
        self.send_header('Content-Type', 'application/json')
        self.send_header('Content-Length', str(len(resp_b)))
        self.end_headers()
        self.wfile.write(resp_b)

    def log_message(self, format, *args):
        # silence logging during tests
        return


def _start_test_server():
    # bind to localhost on ephemeral port
    srv = http.server.HTTPServer(('127.0.0.1', 0), _SilentHandler)
    t = threading.Thread(target=srv.serve_forever, daemon=True)
    t.start()
    # wait a moment for server to be up
    for _ in range(10):
        try:
            sock = socket.create_connection(srv.server_address, timeout=0.5)
            sock.close()
            break
        except Exception:
            time.sleep(0.02)
    return srv, t


def test_upload_to_local_http_server():
    srv, thr = _start_test_server()
    try:
        url = f'http://{srv.server_address[0]}:{srv.server_address[1]}/v1/bugreport'
        diag = {'email': 'bob@example.com', 'home': '/home/bob/secret', 'note': 'contact me at alice@company.org or 10.0.0.1'}
        res = br.upload_bug_report(diag, url)
        assert res.get('id') == 'srv123'

        last = _SilentHandler.server_state.get('last')
        assert last is not None, 'Server did not receive a POST'
        payload = last['data']
        assert 'diagnostics' in payload
        d = payload['diagnostics']
        # email and IP should be redacted in the posted diagnostics
        dumped = json.dumps(d)
        assert 'bob@example.com' not in dumped
        assert 'alice@company.org' not in dumped
        assert '10.0.0.1' not in dumped
        assert '<REDACTED' in dumped
    finally:
        try:
            srv.shutdown()
        except Exception:
            pass
        thr.join(timeout=1)
