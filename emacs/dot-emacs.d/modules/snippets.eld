python-base-mode

(dataclass "@dataclass" n> "class " p ":" n> "\"\"\"" p "\"\"\"" n> p ": " p " = " q)
(dataclass-import "from dataclasses import dataclass" q)
(dataclass-field "from dataclasses import dataclass, field" n> "@dataclass" n> "class " p ":" n> p ": " p " = field(default=" q ")")
(asyncdef "async def " p "(" p "):" n> q)
(asynccontext "@asynccontextmanager" n> "async def " p "(" p "):" n> "yield " q)
(pyfixture "@pytest.fixture" n> "def " p "(" p "):" n> "\"\"\"" p "\"\"\"" n> q)
(pytestcase "def test_" p "(" p "):" n> "\"\"\"" p "\"\"\"" n> q)
(main-guard "def main() -> int:" n> q n n> "if __name__ == \"__main__\":" n> "raise SystemExit(main())")
(httpx-client "async with httpx.AsyncClient() as client:" n> q)
(logging-basic "logger = logging.getLogger(__name__)" n> "logger.setLevel(logging." p ")" n> "handler = logging.StreamHandler()" n> "handler.setFormatter(logging.Formatter(\"%(levelname)s - %(message)s\"))" n> "logger.addHandler(handler)" n> q)
(type-alias "type " p " = " q)
