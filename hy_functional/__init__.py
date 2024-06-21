import hy

def _bootstrap():
    import builtins
    import hy_functional.core as hfc
    builtins.__dict__.update(hfc.select_keys(hfc.__dict__, hfc.__all__))
    builtins._hy_macros.update(hfc.select_keys(hfc._hy_macros, hfc._hy_export_macros))
