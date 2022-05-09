((nil . ((projectile-project-compilation-cmd . "pip install .[dev]")
         (projectile-project-configure-cmd . "pyenv virtualenv 3.10.4 $(basename $PWD) && pyenv local $(basename $PWD) && pip install pre-commit && pre-commit install ")
         (projectile-project-run-cmd . "python -m socket_log_receiver ")
         (projectile-project-test-cmd . "python setup.py tests ")))

 (python-mode . (
                 (eval . (blacken-mode 1))
                 (python-fill-docstring-style . pep-257-nn)
                 (fill-column . 88)
                 )))
