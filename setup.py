from setuptools import setup

setup(
    name='ejperbo',
    version='0.1.0',
    py_modules=['ejperbo'],
    install_requires=[
      'numpy',
      'pandas',
      'tqdm',
      'calendar'
    ],
    entry_points='''
        [console_scripts]
        ejperbo=ejperbo:ejperbo
    ''',
)